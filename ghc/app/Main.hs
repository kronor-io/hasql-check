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

import Control.Monad (unless)
import Data.Char (ord)
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Data.FastString (getLexicalFastString, unpackFS)
import GHC.Iface.Ext.Binary (HieFileResult (..), readHieFile)
import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils (recoverFullType)
import GHC.Iface.Type (IfaceTyCon (..), IfaceTyLit (..))
import GHC.Types.Name (Name, nameModule_maybe, nameOccName)
import GHC.Types.Name.Cache (initNameCache)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.SrcLoc (srcSpanEndCol, srcSpanEndLine, srcSpanStartCol, srcSpanStartLine)
import GHC.Unit.Module (moduleName, moduleNameString)
import Hasql.Check.Ghc.Source (Column (..), Quote (..), Statement (..), Ty (..), statementAt)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)


main :: IO ()
main = do
    files <- getArgs
    nc <- initNameCache 'z' []
    errors <- concat <$> mapM (dump nc) files
    mapM_ (hPutStrLn stderr) errors
    unless (null errors) exitFailure
  where
    dump nc path = do
        result <- readHieFile nc path
        let hf = hie_file_result result
            toTy = convert . flip recoverFullType (hie_types hf)
            found = Map.fromListWith (flip (<>)) [(s, [t]) | (s, t) <- concatMap (collect toTy) (Map.elems (getAsts (hie_asts hf)))]
            source = lines (Text.unpack (Text.decodeUtf8Lenient (hie_hs_src hf)))
        concat
            <$> mapM
                ( \(s, here) ->
                    -- The enclosing declarations hold the same dictionaries,
                    -- but only the quote's own span starts after "[quoter|".
                    case statementAt source (srcSpanStartLine s, srcSpanStartCol s) (srcSpanEndLine s, srcSpanEndCol s) here of
                        Nothing -> pure []
                        Just (Right statement) -> [] <$ putStrLn (output (hie_hs_file hf) statement)
                        Just (Left err) -> pure [hie_hs_file hf <> ":" <> show (srcSpanStartLine s) <> ":" <> show (srcSpanStartCol s) <> ": " <> err]
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


-- | One statement's line of output.
output :: FilePath -> Statement -> String
output file statement =
    object
        [ ("file", string file)
        , ("line", show statement.line)
        , ("column", show statement.quote.column)
        , ("quoter", string statement.quote.quoter)
        , ("sql", string statement.quote.body)
        , ("params", columns statement.params)
        , ("row", maybe "null" columns statement.row)
        ]
  where
    columns cs = "[" <> intercalate "," (map column cs) <> "]"
    column c =
        object
            [ ("type", string c.haskellType)
            , ("postgres", string c.postgres)
            , ("labels", "[" <> intercalate "," (map string c.labels) <> "]")
            , ("nullable", if c.nullable then "true" else "false")
            ]


convert :: HieTypeFix -> Ty
convert (Roll t) = case t of
    HTyConApp tc (HieArgs args) -> uncurry Con (names (ifaceTyConName tc)) [convert a | (True, a) <- args]
    HQualTy _ b -> convert b
    HForAllTy _ b -> convert b
    HCastTy a -> convert a
    HLitTy (IfaceStrTyLit s) -> Lit (unpackFS (getLexicalFastString s))
    HLitTy (IfaceNumTyLit n) -> OtherLit (show n)
    HLitTy (IfaceCharTyLit c) -> OtherLit (show c)
    _ -> Var
  where
    names :: Name -> (String, String)
    names n = (maybe "" (moduleNameString . moduleName) (nameModule_maybe n), occNameString (nameOccName n))


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
