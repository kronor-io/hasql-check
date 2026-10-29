-- |
-- Module      : Main
-- Copyright   : (c) 2026 Kronor.io
--
-- License     : BSD-3-Clause
-- Maintainer  : lorenzo@kronor.io
-- Stability   : experimental
-- Portability : GHC
--
-- Print, for every quote of "Hasql.Check.Inferred.TH" in the given .hie
-- files, the types GHC instantiated its statement at, one JSON object a line:
--
-- > {"file":"src/Shop.hs","line":14,"column":11,"quoter":"maybeStatement",
-- >  "sql":"...","params":[{"type":"GHC.Internal.Int.Int64","nullable":false}],
-- >  "row":[...]}
--
-- for @hasql-check --inferred@. The quote's own span holds the nodes of the
-- code it generated, typed where it is used, even when the statement is an
-- argument with no signature of its own. @row@ is null for a statement
-- returning no rows.
--
-- The .hie format changes with each GHC release: build this with the GHC the
-- project is built with.
module Main where

import Data.ByteString.Char8 qualified as B
import Data.Char (isAlphaNum, ord)
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Iface.Ext.Binary (HieFileResult (..), readHieFile)
import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils (recoverFullType)
import GHC.Iface.Type (IfaceTyCon (..))
import GHC.Types.Name (Name, nameModule_maybe, nameOccName)
import GHC.Types.Name.Cache (initNameCache)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.SrcLoc (RealSrcSpan, srcSpanEndCol, srcSpanEndLine, srcSpanStartCol, srcSpanStartLine)
import GHC.Unit.Module (moduleName, moduleNameString)
import System.Environment (getArgs)
import Text.Printf (printf)


data Ty = Con String String [Ty] | Var


main :: IO ()
main = do
    files <- getArgs
    nc <- initNameCache 'z' []
    mapM_ (dump nc) files
  where
    dump nc path = do
        result <- readHieFile nc path
        let hf = hie_file_result result
            toTy = convert . flip recoverFullType (hie_types hf)
            found = Map.fromListWith (flip (<>)) [(s, [t]) | (s, t) <- concatMap (collect toTy) (Map.elems (getAsts (hie_asts hf)))]
            src = hie_hs_src hf
        mapM_
            ( \(s, here) ->
                let params = [p | Con "Hasql.Check.Inferred" "Params" [p] <- here, ground p]
                    rows = [r | Con "Hasql.Check.Inferred" "Row" [r] <- here, ground r]
                 in case params of
                        p : _ | isQuote src s -> putStrLn (quote (hie_hs_file hf) src s p (safeHead rows))
                        _ -> pure ()
            )
            (Map.toList found)

    collect toTy node =
        -- The dictionaries the generated code passes are identifiers of the
        -- quote's node, typed by the instance GHC chose.
        let infos = Map.elems (getSourcedNodeInfo (sourcedNodeInfo node))
            here =
                [(nodeSpan node, toTy t) | info <- infos, t <- nodeType info]
                    <> [(nodeSpan node, toTy t) | info <- infos, (Right _, details) <- Map.toList (nodeIdentifiers info), Just t <- [identType details]]
         in here <> concatMap (collect toTy) (nodeChildren node)

    safeHead (x : _) = Just x
    safeHead [] = Nothing


-- | Whether the span is a quote's: whether it starts right after "[quoter|".
-- The enclosing declarations hold the same dictionaries.
isQuote :: B.ByteString -> RealSrcSpan -> Bool
isQuote src s =
    let lineText = lines (Text.unpack (Text.decodeUtf8Lenient src)) !! (srcSpanStartLine s - 1)
     in srcSpanStartCol s >= 2 && lineText !! (srcSpanStartCol s - 2) == '|'


-- | One quote's line of output.
quote :: FilePath -> B.ByteString -> RealSrcSpan -> Ty -> Maybe Ty -> String
quote file src s params row =
    object
        [ ("file", string file)
        , ("line", show line)
        , ("column", show (quoteColumn + 1))
        , ("quoter", string quoter)
        , ("sql", string body)
        , ("params", columns True params)
        , ("row", maybe "null" (columns False) row)
        ]
  where
    srcLines = lines (Text.unpack (Text.decodeUtf8Lenient src))
    line = srcSpanStartLine s
    lineText = srcLines !! (line - 1)
    -- The span starts at the quote's body, after "[quoter|".
    bodyStart = srcSpanStartCol s - 1
    quoteColumn = lastBracket (take bodyStart lineText)
    quoter = takeWhile (\c -> isAlphaNum c || c `elem` ("._'" :: String)) (drop (quoteColumn + 1) lineText)
    -- The span ends after the closing "|]".
    body = slice (line, srcSpanStartCol s) (srcSpanEndLine s, srcSpanEndCol s - 2)
    slice (l1, c1) (l2, c2)
        | l1 == l2 = take (c2 - c1) (drop (c1 - 1) (srcLines !! (l1 - 1)))
        | otherwise =
            intercalate "\n" $
                [drop (c1 - 1) (srcLines !! (l1 - 1))]
                    <> [srcLines !! (l - 1) | l <- [l1 + 1 .. l2 - 1]]
                    <> [take (c2 - 1) (srcLines !! (l2 - 1))]
    lastBracket prefix = case [i | (i, '[') <- zip [0 ..] prefix] of
        [] -> 0
        is -> last is


-- | A parameter or row type, as the list of its columns.
columns :: Bool -> Ty -> String
columns isParams = \case
    Con _ "Unit" [] | isParams -> "[]"
    Con _ name args | take 5 name == "Tuple" -> list (map column args)
    t -> list [column t]
  where
    column = \case
        Con _ "Maybe" [t] -> one t True
        t -> one t False
    one t nullable = object [("type", string (qualified t)), ("nullable", if nullable then "true" else "false")]
    qualified = \case
        Con m n _ -> if null m then n else m <> "." <> n
        Var -> "?"
    list xs = "[" <> intercalate "," xs <> "]"


convert :: HieTypeFix -> Ty
convert (Roll t) = case t of
    HTyConApp tc (HieArgs args) -> uncurry Con (names (ifaceTyConName tc)) [convert a | (True, a) <- args]
    HQualTy _ b -> convert b
    HForAllTy _ b -> convert b
    HCastTy a -> convert a
    _ -> Var
  where
    names :: Name -> (String, String)
    names n = (maybe "" (moduleNameString . moduleName) (nameModule_maybe n), occNameString (nameOccName n))


ground :: Ty -> Bool
ground = \case
    Var -> False
    Con _ _ args -> all ground args


object :: [(String, String)] -> String
object fields = "{" <> intercalate "," [string k <> ":" <> v | (k, v) <- fields] <> "}"


string :: String -> String
string s = "\"" <> concatMap escape s <> "\""
  where
    escape = \case
        '"' -> "\\\""
        '\\' -> "\\\\"
        '\n' -> "\\n"
        '\t' -> "\\t"
        c | ord c < 0x20 -> printf "\\u%04x" (ord c)
        c -> [c]
