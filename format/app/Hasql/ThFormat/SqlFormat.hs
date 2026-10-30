{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Hasql.ThFormat.SqlFormat (
    formatSql,
)
where

import Data.Char (toLower)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import PostgresqlSyntax qualified as Ast
import Prettyprinter (Doc, LayoutOptions (..), PageWidth (..), align, hardline, hcat, indent, layoutPretty, nest, pretty, punctuate, vsep, (<+>))
import Prettyprinter.Render.Text (renderStrict)
import TextBuilder (TextBuilder)
import TextBuilder qualified
import Prelude


-- | postgresql-syntax 0.5 wraps every comma-separated production in its own
-- newtype where 0.4 used a bare @NonEmpty@. Rather than unwrap at each of the
-- forty-odd call sites, 'toL' names "give me the elements" once and covers
-- both shapes.
class ToList a b | a -> b where
    toL :: a -> [b]


instance ToList (NE.NonEmpty a) a where
    toL = NE.toList


instance ToList Ast.ArrayExprList Ast.ArrayExpr where toL (Ast.ArrayExprList xs) = NE.toList xs
instance ToList Ast.Attrs Ast.Ident where toL (Ast.Attrs xs) = NE.toList xs
instance ToList Ast.ExprList Ast.AExpr where toL (Ast.ExprList xs) = NE.toList xs
instance ToList Ast.FromList Ast.TableRef where toL (Ast.FromList xs) = NE.toList xs
instance ToList Ast.FromClause Ast.TableRef where toL (Ast.FromClause xs) = toL xs
instance ToList Ast.GroupClause Ast.GroupByItem where toL (Ast.GroupClause xs) = NE.toList xs
instance ToList Ast.Indirection Ast.IndirectionEl where toL (Ast.Indirection xs) = NE.toList xs
#ifdef KRONOR_SYNTAX
instance ToList Ast.HsTargetList Ast.HsTargetEl where toL (Ast.HsTargetList xs) = NE.toList xs
#endif
instance ToList Ast.SetClauseList Ast.SetClause where toL (Ast.SetClauseList xs) = NE.toList xs
instance ToList Ast.SortClause Ast.SortBy where toL (Ast.SortClause xs) = NE.toList xs
instance ToList Ast.RowsfromList Ast.RowsfromItem where toL (Ast.RowsfromList xs) = NE.toList xs
instance ToList Ast.NameList Ast.Ident where toL (Ast.NameList xs) = NE.toList xs
instance ToList Ast.IndexParams Ast.IndexElem where toL (Ast.IndexParams xs) = NE.toList xs
instance ToList Ast.InsertColumnList Ast.InsertColumnItem where toL (Ast.InsertColumnList xs) = NE.toList xs
instance ToList Ast.TableFuncElementList Ast.TableFuncElement where toL (Ast.TableFuncElementList xs) = NE.toList xs
instance ToList Ast.TargetList Ast.TargetEl where toL (Ast.TargetList xs) = NE.toList xs
instance ToList Ast.ValuesClause Ast.ExprList where toL (Ast.ValuesClause xs) = NE.toList xs
instance ToList Ast.SetTargetList Ast.SetTarget where toL (Ast.SetTargetList xs) = NE.toList xs
instance ToList Ast.TypeList Ast.Typename where toL (Ast.TypeList xs) = NE.toList xs
instance ToList Ast.WhenClauseList Ast.WhenClause where toL (Ast.WhenClauseList xs) = NE.toList xs
instance ToList Ast.WindowClause Ast.WindowDefinition where toL (Ast.WindowClause xs) = NE.toList xs


-- | Indentation size: 4 spaces per level.
indentSize :: Int
indentSize = 4


-- | Layout options: unbounded width so explicit `line`/`hardline` are the only
-- source of line breaks; the layout never tries to flatten or reflow.
layoutOpts :: LayoutOptions
layoutOpts = LayoutOptions Unbounded


-- | Format SQL content. qqIndent is the 0-based column of the quasiquote
-- opening bracket. The formatted output starts with a leading newline (so the
-- first SQL line is on its own line under the bracket) and ends with a newline
-- + qqIndent spaces (so the closing `|]` lines up under the opening bracket).
--
-- Returns @Right formatted@ on success, @Left reason@ if the formatter
-- emitted an AST-divergent rewrite. Callers should surface @Left@ as a
-- hard failure (the file is left untouched; callers exit non-zero rather
-- than silently committing a renderer bug). Inputs the parser rejects are
-- not considered an error — they round-trip as @Right input@ since the
-- formatter never tried to rewrite them.
formatSql :: Int -> Text -> Either Text Text
formatSql qqIndent sql
    | T.null (T.strip sql) = Right sql
    -- Single-line queries (no newlines) are left untouched.
    | not (T.isInfixOf "\n" sql) = Right sql
    | otherwise =
        case parseStmt (T.strip sql) of
            Left _ -> Right sql
            Right ast ->
                let doc :: Doc ()
                    doc = renderPreparableStmt ast
                    body = renderStrict (layoutPretty layoutOpts (indent (qqIndent + indentSize) doc))
                    qqIndentText = T.replicate qqIndent " "
                    formatted = "\n" <> body <> "\n" <> qqIndentText
                 in -- Round-trip safety: compare the re-parsed AST to the original.
                    -- Parse-only equivalence isn't enough — the renderer could drop
                    -- or alter a clause and still produce valid SQL (see the
                    -- `DistinctTargeting` regression that motivated this check).
                    case parseStmt (T.strip formatted) of
                        Right ast' | ast' == ast -> Right formatted
                        Right _ ->
                            Left "AST mismatch: formatter rendered semantically-different SQL (renderer bug)"
                        Left err ->
                            Left ("AST mismatch: formatter output did not re-parse (" <> err <> ")")


-- | The formatter reads and writes the SQL that lives inside a hasql-th
-- quasiquote, so every Kronor extension has to survive the round trip:
-- @$1 :: int8?@ nullability markers, @$f (..)@ \/ @$R {..}@ Haskell targets
-- and @$1.$field@ Haskell param fields. Both the parser and the renderer run
-- with all three enabled, unlike the quasiquoter itself, which lowers and
-- erases them on the way to Postgres.
settings :: Ast.Settings
settings =
    mconcat
        [ Ast.nullabilityMarkers True
#ifdef KRONOR_SYNTAX
        , Ast.haskellTargets True
        , Ast.haskellParamFields True
#endif
        ]


-- | Parse a statement with the extension syntax enabled.
parseStmt :: Text -> Either Text Ast.PreparableStmt
parseStmt = Ast.parse settings


-- | Render any AST node with postgresql-syntax's own renderer, extensions on.
render :: Ast.IsAst a => a -> TextBuilder
render = Ast.toTextBuilder settings


-- | 'Ast.VerbalExprBinOp' renders without its @NOT@, which lives in the
-- enclosing 'Ast.VerbalExprBinOpAExpr'. Mirrors postgresql-syntax's own
-- rendering of that node.
verbalExprBinOp :: Bool -> Ast.VerbalExprBinOp -> TextBuilder
verbalExprBinOp negated op = (if negated then "NOT " else mempty) <> render op


-- | Same story for 'Ast.AExprReversableOp': the negation is external to the
-- type, and each alternative places it differently (@IS NOT NULL@ but
-- @NOT BETWEEN@). Mirrors postgresql-syntax's @renderAExprReversableOp@.
aExprReversableOp :: Bool -> Ast.AExprReversableOp -> TextBuilder
aExprReversableOp negated = \case
    Ast.NullAExprReversableOp -> isNot <> "NULL"
    Ast.TrueAExprReversableOp -> isNot <> "TRUE"
    Ast.FalseAExprReversableOp -> isNot <> "FALSE"
    Ast.UnknownAExprReversableOp -> isNot <> "UNKNOWN"
    Ast.DistinctFromAExprReversableOp a -> isNot <> "DISTINCT FROM " <> render a
    Ast.OfAExprReversableOp a -> isNot <> "OF (" <> render a <> ")"
    Ast.BetweenAExprReversableOp asymmetric a b ->
        notPfx <> (if asymmetric then "BETWEEN ASYMMETRIC " else "BETWEEN ") <> render a <> " AND " <> render b
    Ast.BetweenSymmetricAExprReversableOp a b ->
        notPfx <> "BETWEEN SYMMETRIC " <> render a <> " AND " <> render b
    Ast.InAExprReversableOp a -> notPfx <> "IN " <> render a
    Ast.DocumentAExprReversableOp -> isNot <> "DOCUMENT"
  where
    isNot = if negated then "IS NOT " else "IS "
    notPfx = if negated then "NOT " else mempty


-- | 0.5 models @ROW ()@ as its own 'Ast.ExplicitRow' type where 0.4 used a
-- @Maybe@ of the expression list. The formatter still branches on the @Maybe@.
explicitRowExprs :: Ast.ExplicitRow -> Maybe Ast.ExprList
explicitRowExprs = \case
    Ast.EmptyExplicitRow -> Nothing
    Ast.ExprListExplicitRow xs -> Just xs


-- | JSON/JSONB access operators rendered without surrounding spaces, since
-- they act like field accessors.
jsonOps :: [Text]
jsonOps = ["->", "->>", "#>", "#>>"]


-- | Render via postgresql-syntax's renderer as a 'Doc'. Output is lowercased
-- and @:: @ typecast spacing is tightened to @::@, while content inside
-- single-quoted string literals (@'...'@) and double-quoted identifiers
-- (@"..."@) is preserved verbatim.
inline :: (a -> TextBuilder) -> a -> Doc ann
inline f = pretty . inlineText f


-- | Same as 'inline' but stays at the Text layer. Useful when we need to do
-- a string comparison on the rendered output (e.g. detecting a JSON access
-- operator) before lifting it into a 'Doc'.
inlineText :: (a -> TextBuilder) -> a -> Text
inlineText f = inlineNormalize . TextBuilder.toText . f


-- | Lowercase keywords and tighten @ :: @ to @::@ in a single pass, while
-- preserving the contents of single-quoted string literals and double-quoted
-- identifiers verbatim. Standard SQL doubled-quote escapes (@''@ and @""@)
-- are recognised so the scanner stays inside the literal until the real
-- closing quote.
inlineNormalize :: Text -> Text
inlineNormalize = T.pack . goCode . T.unpack
  where
    goCode :: String -> String
    goCode [] = []
    goCode ('\'' : rest) = '\'' : goString '\'' rest
    goCode ('"' : rest) = '"' : goString '"' rest
    goCode (' ' : ':' : ':' : ' ' : rest) = "::" ++ goCode rest
    goCode (c : rest) = toLower c : goCode rest

    goString :: Char -> String -> String
    goString _ [] = []
    -- Doubled-quote escape: stay inside the literal.
    goString q (c1 : c2 : rest) | c1 == q && c2 == q = c1 : c2 : goString q rest
    goString q (c : rest)
        | c == q = c : goCode rest
        | otherwise = c : goString q rest


----------------------------------------------------------------------
-- Expression / leaf renderers (return 'Doc ann' — inline single-line by
-- construction; structural renderers below use the same type for
-- multi-line layouts).
----------------------------------------------------------------------

renderExpr :: Ast.AExpr -> Doc ann
renderExpr = \case
    Ast.TypecastAExpr expr tn ->
        renderExpr expr <> "::" <> renderTypename tn
    Ast.AndAExpr left right ->
        renderExpr left <+> "and" <+> renderExpr right
    Ast.OrAExpr left right ->
        renderExpr left <+> "or" <+> renderExpr right
    Ast.NotAExpr expr ->
        "not" <+> renderExpr expr
    Ast.CExprAExpr cexpr -> renderCExpr cexpr
    Ast.SymbolicBinOpAExpr left op right ->
        let opText = inlineText render op
            opDoc = pretty opText
         in if opText `elem` jsonOps
                then renderExpr left <> opDoc <> renderExpr right
                else renderExpr left <+> opDoc <+> renderExpr right
    Ast.PlusAExpr expr -> "+" <> renderExpr expr
    Ast.MinusAExpr expr -> "-" <> renderExpr expr
    Ast.ReversableOpAExpr expr negated op ->
        renderExpr expr <+> renderReversableOp negated op
    Ast.IsnullAExpr expr -> renderExpr expr <+> "isnull"
    Ast.NotnullAExpr expr -> renderExpr expr <+> "notnull"
    Ast.CollateAExpr expr name -> renderExpr expr <+> "collate" <+> inline render name
    Ast.AtTimeZoneAExpr expr tz -> renderExpr expr <+> "at time zone" <+> renderExpr tz
    Ast.PrefixQualOpAExpr op expr -> inline render op <+> renderExpr expr
    Ast.VerbalExprBinOpAExpr left negated op right mEscape ->
        renderExpr left
            <+> inline (verbalExprBinOp negated) op
            <+> renderExpr right
            <> maybe mempty (\e -> " escape" <+> renderExpr e) mEscape
    Ast.SubqueryAExpr expr op subType subq ->
        renderExpr expr
            <+> inline render op
            <+> inline render subType
            <> either
                ((" " <>) . renderSelectWithParensMaybeWrap)
                (\e -> "(" <> renderExprMaybeWrap e <> ")")
                subq
    Ast.DefaultAExpr -> "default"
    Ast.UniqueAExpr swp -> "unique" <+> renderSelectWithParensMaybeWrap swp
    Ast.OverlapsAExpr r1 r2 -> inline render r1 <+> "overlaps" <+> inline render r2


renderCExpr :: Ast.CExpr -> Doc ann
renderCExpr = \case
    Ast.ParamCExpr n mInd ->
        "$" <> pretty n <> maybe mempty renderIndirection mInd
    Ast.InParensCExpr expr mInd ->
        "(" <> renderExpr expr <> ")" <> maybe mempty renderIndirection mInd
    Ast.FuncCExpr fe -> renderFuncExpr fe
    Ast.ColumnrefCExpr cr -> inline render cr
    Ast.AexprConstCExpr ac -> inline render ac
    Ast.SelectWithParensCExpr swp mInd ->
        renderSelectWithParensMaybeWrap swp <> maybe mempty renderIndirection mInd
    Ast.ExistsCExpr swp -> "exists" <+> renderSelectWithParensMaybeWrap swp
    Ast.CaseCExpr caseExpr -> renderCaseExpr caseExpr
    Ast.ArrayCExpr arr -> "array" <> either renderSelectWithParensMaybeWrap renderArrayExpr arr
    Ast.ExplicitRowCExpr row ->
        "row(" <> maybe mempty (commaJoin . map renderExpr . toL) (explicitRowExprs row) <> ")"
    Ast.ImplicitRowCExpr (Ast.ImplicitRow exprs expr) ->
        "(" <> commaJoin (map renderExpr (toL exprs) ++ [renderExpr expr]) <> ")"
    other -> inline render other


-- | Comma-and-space-separated concatenation of inline 'Doc's. Used wherever
-- the previous Text-based renderer used 'T.intercalate ", "'.
commaJoin :: [Doc ann] -> Doc ann
commaJoin = hcat . punctuate ", "


renderArrayExpr :: Ast.ArrayExpr -> Doc ann
renderArrayExpr = \case
    Ast.ExprListArrayExpr exprs ->
        "[" <> commaJoin (map renderExpr (toL exprs)) <> "]"
    Ast.ArrayExprListArrayExpr nested ->
        "[" <> commaJoin (map renderArrayExpr (toL nested)) <> "]"
    Ast.EmptyArrayExpr -> "[]"


renderCaseExpr :: Ast.CaseExpr -> Doc ann
renderCaseExpr (Ast.CaseExpr mArg whenClauses mDefault) =
    "case"
        <> maybe mempty (\arg -> " " <> renderExpr arg) mArg
        <> mconcat [" " <> renderWhenClause w | w <- toL whenClauses]
        <> maybe mempty (\d -> " else" <+> renderExpr d) mDefault
        <+> "end"


renderWhenClause :: Ast.WhenClause -> Doc ann
renderWhenClause (Ast.WhenClause cond result) =
    "when" <+> renderExpr cond <+> "then" <+> renderExpr result


renderIndirection :: Ast.Indirection -> Doc ann
renderIndirection = mconcat . map renderIndirectionEl . toL


renderIndirectionEl :: Ast.IndirectionEl -> Doc ann
renderIndirectionEl = \case
    Ast.AttrNameIndirectionEl ident -> "." <> renderIdent ident
    Ast.AllIndirectionEl -> ".*"
    Ast.ExprIndirectionEl expr -> "[" <> renderExpr expr <> "]"
    Ast.SliceIndirectionEl mFrom mTo ->
        "[" <> maybe mempty renderExpr mFrom <> ":" <> maybe mempty renderExpr mTo <> "]"
#ifdef KRONOR_SYNTAX
    -- Kronor extension: a Haskell field selector on a @$n@ placeholder.
    Ast.HsAttrNameIndirectionEl t -> ".$" <> pretty t
#endif


renderIdent :: Ast.Ident -> Doc ann
renderIdent = \case
    Ast.QuotedIdent t -> "\"" <> pretty t <> "\""
    Ast.UnquotedIdent t -> pretty t


renderFuncExpr :: Ast.FuncExpr -> Doc ann
renderFuncExpr = \case
    Ast.ApplicationFuncExpr app mWithin mFilter mOver ->
        renderFuncApplication app
            <> maybe mempty (\w -> " " <> inline render w) mWithin
            <> maybe mempty (\f -> " filter (where" <+> renderExpr f <> ")") mFilter
            <> maybe mempty (\o -> " " <> inline render o) mOver
    Ast.SubexprFuncExpr sub -> renderFuncExprCommonSubexpr sub


renderFuncApplication :: Ast.FuncApplication -> Doc ann
renderFuncApplication (Ast.FuncApplication name mParams) =
    inline render name <> "(" <> maybe mempty renderFuncAppParams mParams <> ")"


-- | Threshold (number of positional arguments) at which a function call's
-- argument list is broken onto its own line per argument. A call with
-- fewer args than this threshold renders inline; one with this many or more
-- wraps. Currently set to 2, so single-arg calls (e.g. @count(*)@,
-- @md5(x)@, @unnest(arr)@) stay inline.
funcWrapThreshold :: Int
funcWrapThreshold = 2


-- | Number of positional arguments in a function application (variadic args
-- count as one each, star/none count as zero).
funcAppArgCount :: Ast.FuncApplication -> Int
funcAppArgCount (Ast.FuncApplication _ mParams) = case mParams of
    Nothing -> 0
    Just (Ast.NormalFuncApplicationParams _ args _) -> length (toL args)
    Just (Ast.VariadicFuncApplicationParams mArgs _ _) -> maybe 0 (length . toL) mArgs + 1
    Just Ast.StarFuncApplicationParams -> 0


-- | Multi-line rendering of a function application using the leading-comma
-- block-list layout that's standard elsewhere in this formatter:
--
-- @
-- fn(
--       arg1
--     , arg2
--     , arg3
-- )
-- @
--
-- The args sit at @outer + indentSize + 2@ (first) and @outer + indentSize@
-- (`, ` prefix on subsequent), where @outer@ is the surrounding nesting
-- level. The closing paren returns to @outer@; this can put it visually
-- left of the function name when the function call is itself indented
-- (e.g. as a leading-comma item), but it keeps the layout predictable.
renderFuncApplicationWrapped :: Ast.FuncApplication -> Doc ann
renderFuncApplicationWrapped (Ast.FuncApplication name mParams) =
    inline render name
        <> "("
        <> nest indentSize (maybe mempty wrappedBody mParams)
        <> hardline
        <> ")"
  where
    wrappedBody params = case params of
        Ast.NormalFuncApplicationParams mDistinct args mSort ->
            let argList = toL args
                argDocs = map renderFuncArgExprMaybeWrap argList
                prefix = maybe mempty (\b -> if b then "distinct " else "all ") mDistinct
                rows =
                    if looksLikeKeyValuePairs argList
                        then renderArgPairRows argDocs
                        else renderArgRows argDocs
                sortDoc = maybe mempty (\sc -> hardline <> inline render sc) mSort
             in prefix <> rows <> sortDoc
        Ast.VariadicFuncApplicationParams mArgs varArg mSort ->
            let regular = maybe [] (map renderFuncArgExprMaybeWrap . toL) mArgs
                variadic = "variadic " <> renderFuncArgExprMaybeWrap varArg
                rows = renderArgRows (regular ++ [variadic])
                sortDoc = maybe mempty (\sc -> hardline <> inline render sc) mSort
             in rows <> sortDoc
        Ast.StarFuncApplicationParams -> hardline <> "*"


-- | First arg: hardline, then 2 extra spaces (so arg sits at col + 6), then
-- the arg. Subsequent args: hardline, then @, @ (so the comma sits at col +
-- 4 and the arg content also at col + 6). Used by every wrapped renderer
-- that emits a leading-comma block-list.
renderArgRows :: [Doc ann] -> Doc ann
renderArgRows = \case
    [] -> mempty
    (x : xs) ->
        (hardline <> "  " <> x)
            <> mconcat [hardline <> ", " <> r | r <- xs]


-- | Pair layout for key/value-shaped arg lists. Each pair lives on its own
-- line (key + inline @, @ + value). The first pair uses the same "  " row
-- prefix as 'renderArgRows'; subsequent pairs use the leading @, @ row
-- separator. Caller guarantees an even-length input.
renderArgPairRows :: [Doc ann] -> Doc ann
renderArgPairRows = goFirst . pairs
  where
    pairs (a : b : rest) = (a, b) : pairs rest
    pairs _ = []

    goFirst [] = mempty
    goFirst ((k, v) : rest) =
        (hardline <> "  " <> k <> ", " <> v)
            <> mconcat [hardline <> ", " <> k' <> ", " <> v' | (k', v') <- rest]


-- | If an expression is a top-level function call (optionally wrapped in any
-- number of chained typecasts), return the function application along with a
-- function that re-attaches the typecasts to a Doc, preserving their order.
-- Used to detect "wrap-eligible" expressions in select target lists.
topLevelFuncApp :: Ast.AExpr -> Maybe (Ast.FuncApplication, Doc ann -> Doc ann)
topLevelFuncApp = \case
    Ast.TypecastAExpr inner tn
        | Just (app, attach) <- topLevelFuncApp inner ->
            Just (app, \d -> attach d <> "::" <> renderTypename tn)
    Ast.CExprAExpr (Ast.FuncCExpr (Ast.ApplicationFuncExpr app Nothing Nothing Nothing)) ->
        Just (app, id)
    _ -> Nothing


-- | Heuristic: an arg list "looks like" alternating key/value pairs when it
-- has at least 4 args, an even count, and every even-indexed argument
-- (positions 0, 2, 4, ...) is a string-literal expression. This catches
-- @jsonb_build_object('k1', v1, 'k2', v2, ...)@ and friends without naming
-- specific functions, so the layout adapts to anything with the same shape.
looksLikeKeyValuePairs :: [Ast.FuncArgExpr] -> Bool
looksLikeKeyValuePairs args =
    let n = length args
     in even n && n >= 4 && all isLiteralKey (zip [0 :: Int ..] args)
  where
    isLiteralKey (i, arg)
        | even i = case arg of
            Ast.ExprFuncArgExpr e -> isStringLiteral e
            _ -> False
        | otherwise = True

    isStringLiteral :: Ast.AExpr -> Bool
    isStringLiteral = \case
        Ast.CExprAExpr (Ast.AexprConstCExpr (Ast.SAexprConst _)) -> True
        _ -> False


-- | An expression is "wrap-eligible" when it has a structural shape the
-- multi-line renderers know how to lay out: a function call (optionally
-- under chained typecasts), an @array[...]@ literal whose elements
-- themselves contain wrap-eligible content, a @row(...)@ / @(a, b, c)@
-- tuple, a parenthesized expression, a @case@ expression, or one of the
-- SQL keyword-form pseudo-functions (@coalesce@, @cast@, @nullif@,
-- @greatest@, @least@, @position@, @substring@, @trim@, @overlay@,
-- @treat@). The decision to actually wrap (vs render inline) is made by
-- 'renderFuncArgExprMaybeWrap' / 'renderExprMaybeWrap' based on the
-- inline rendered length.
isWrapEligibleExpr :: Ast.AExpr -> Bool
isWrapEligibleExpr = \case
    Ast.TypecastAExpr inner _ -> isWrapEligibleExpr inner
    Ast.CExprAExpr cexpr -> isWrapEligibleCExpr cexpr
    _ -> False


isWrapEligibleCExpr :: Ast.CExpr -> Bool
isWrapEligibleCExpr = \case
    Ast.FuncCExpr (Ast.ApplicationFuncExpr _ Nothing Nothing Nothing) -> True
    Ast.FuncCExpr (Ast.SubexprFuncExpr sub) -> isWrapEligibleSubexpr sub
    Ast.ArrayCExpr (Right arr) -> isWrapEligibleArray arr
    Ast.InParensCExpr inner _ -> isWrapEligibleExpr inner
    Ast.ExplicitRowCExpr row ->
        any isWrapEligibleExpr (maybe [] toL (explicitRowExprs row))
    Ast.ImplicitRowCExpr (Ast.ImplicitRow exprs expr) ->
        any isWrapEligibleExpr (toL exprs) || isWrapEligibleExpr expr
    -- @case ... when ... end@ is always wrap-eligible: even when the
    -- WHEN/THEN bodies are simple, the multi-line layout reads much
    -- better once the inline form gets long (e.g. multi-clause SET RHS
    -- assignments). The length gate decides when to actually wrap.
    Ast.CaseCExpr _ -> True
    _ -> False


-- | An array literal is wrap-eligible when any of its elements is itself
-- wrap-eligible (so a long @array[fn(...)]@ expands to multi-line by
-- propagating the wrap into the contained function call).
isWrapEligibleArray :: Ast.ArrayExpr -> Bool
isWrapEligibleArray = \case
    Ast.ExprListArrayExpr exprs -> any isWrapEligibleExpr (toL exprs)
    Ast.ArrayExprListArrayExpr nested -> any isWrapEligibleArray (toL nested)
    Ast.EmptyArrayExpr -> False


-- | The SQL keyword-form pseudo-functions (@coalesce@, @cast@, @nullif@,
-- @greatest@, @least@, @position@, @substring@, @trim@, @overlay@,
-- @treat@, @extract@) are always wrap-eligible — when their inline form
-- gets too long, the wrapped layout puts each argument on its own line
-- using 'renderArgRows'.
isWrapEligibleSubexpr :: Ast.FuncExprCommonSubexpr -> Bool
isWrapEligibleSubexpr = \case
    Ast.CoalesceFuncExprCommonSubexpr _ -> True
    Ast.CastFuncExprCommonSubexpr _ _ -> True
    Ast.NullIfFuncExprCommonSubexpr _ _ -> True
    Ast.GreatestFuncExprCommonSubexpr _ -> True
    Ast.LeastFuncExprCommonSubexpr _ -> True
    Ast.PositionFuncExprCommonSubexpr _ -> True
    Ast.SubstringFuncExprCommonSubexpr _ -> True
    Ast.TrimFuncExprCommonSubexpr _ _ -> True
    Ast.OverlayFuncExprCommonSubexpr _ -> True
    Ast.TreatFuncExprCommonSubexpr _ _ -> True
    Ast.ExtractFuncExprCommonSubexpr _ -> True
    _ -> False


-- | Inline-length threshold above which a wrap-eligible sub-expression
-- switches from inline to multi-line rendering inside an already-wrapped
-- function call. Short calls like @jsonb_build_object('a', $1)@ stay on
-- one line; long ones like
-- @array_agg(mk_payload(caller => '...', payload => ...))::jsonb[]@ break
-- onto multiple lines, recursing into their own arguments.
inlineLengthThreshold :: Int
inlineLengthThreshold = 60


-- | Wrap-aware variant of 'renderFuncArgExpr'. Used inside
-- 'renderFuncApplicationWrapped' so that nested function-call arguments
-- expand to multi-line when their inline form exceeds
-- 'inlineLengthThreshold', and stay inline otherwise. Pair-shaped
-- sub-calls (e.g. nested @jsonb_build_object(...)@) always wrap when
-- they appear inside an already-wrapped parent — keeping the visual
-- consistency of the @(key, value)@ pairs even when the inner call's
-- inline form would fit under the threshold.
renderFuncArgExprMaybeWrap :: Ast.FuncArgExpr -> Doc ann
renderFuncArgExprMaybeWrap arg =
    let inlineDoc = renderFuncArgExpr arg
        inlineLen = T.length (renderToText inlineDoc)
        forceWrap = isPairShapedArg arg
     in if (inlineLen > inlineLengthThreshold || forceWrap) && argIsWrapEligible arg
            then renderFuncArgExprWrapped arg
            else inlineDoc
  where
    argIsWrapEligible :: Ast.FuncArgExpr -> Bool
    argIsWrapEligible = \case
        Ast.ExprFuncArgExpr e -> isWrapEligibleExpr e
        Ast.ColonEqualsFuncArgExpr _ e -> isWrapEligibleExpr e
        Ast.EqualsGreaterFuncArgExpr _ e -> isWrapEligibleExpr e

    isPairShapedArg :: Ast.FuncArgExpr -> Bool
    isPairShapedArg = \case
        Ast.ExprFuncArgExpr e -> hasPairShapedCall e
        Ast.ColonEqualsFuncArgExpr _ e -> hasPairShapedCall e
        Ast.EqualsGreaterFuncArgExpr _ e -> hasPairShapedCall e


-- | True when the expression is a function call (optionally under chained
-- typecasts) whose argument list satisfies 'looksLikeKeyValuePairs'. Used
-- to force-wrap nested @jsonb_build_object@-shaped values so a wrapped
-- pair-grouped parent doesn't have inline pair-grouped children
-- (visually inconsistent).
hasPairShapedCall :: Ast.AExpr -> Bool
hasPairShapedCall = \case
    Ast.TypecastAExpr inner _ -> hasPairShapedCall inner
    Ast.CExprAExpr (Ast.FuncCExpr (Ast.ApplicationFuncExpr (Ast.FuncApplication _ (Just (Ast.NormalFuncApplicationParams _ args _))) Nothing Nothing Nothing)) ->
        looksLikeKeyValuePairs (toL args)
    _ -> False


-- | Render a wrap-eligible 'FuncArgExpr' multi-line. The named-arg prefix
-- (@name := @ / @name => @) stays on the same line as the function name and
-- @(@; only the function body wraps.
renderFuncArgExprWrapped :: Ast.FuncArgExpr -> Doc ann
renderFuncArgExprWrapped = \case
    Ast.ExprFuncArgExpr e -> renderExprWrapped e
    Ast.ColonEqualsFuncArgExpr ident e -> inline render ident <> " := " <> renderExprWrapped e
    Ast.EqualsGreaterFuncArgExpr ident e -> inline render ident <> " => " <> renderExprWrapped e


-- | Render a wrap-eligible expression multi-line: peel chained typecasts,
-- then dispatch the underlying function application through
-- 'renderFuncApplicationWrapped' (or @array[...]@ literals through
-- 'renderArrayExprWrapped'). Recursion into deeper levels happens via
-- those wrappers calling 'renderFuncArgExprMaybeWrap' /
-- 'renderExprMaybeWrap' on their own elements.
renderExprWrapped :: Ast.AExpr -> Doc ann
renderExprWrapped = \case
    Ast.TypecastAExpr inner tn ->
        renderExprWrapped inner <> "::" <> renderTypename tn
    Ast.CExprAExpr (Ast.FuncCExpr (Ast.ApplicationFuncExpr app Nothing Nothing Nothing)) ->
        renderFuncApplicationWrapped app
    Ast.CExprAExpr (Ast.FuncCExpr (Ast.SubexprFuncExpr sub)) ->
        renderFuncExprCommonSubexprWrapped sub
    Ast.CExprAExpr (Ast.ArrayCExpr (Right arr)) ->
        "array" <> renderArrayExprWrapped arr
    Ast.CExprAExpr (Ast.InParensCExpr inner mInd) ->
        renderInParensWrapped inner mInd
    Ast.CExprAExpr (Ast.ExplicitRowCExpr row) ->
        renderExplicitRowWrapped (explicitRowExprs row)
    Ast.CExprAExpr (Ast.ImplicitRowCExpr row) ->
        renderImplicitRowWrapped row
    Ast.CExprAExpr (Ast.CaseCExpr caseExpr) ->
        renderCaseExprWrapped caseExpr
    other -> renderExpr other


-- | Multi-line @(expr)@: open paren on the current line, inner expression
-- nested one level deeper on its own block, closing paren back at the
-- caller's column. The optional indirection (@.field@, @[index]@) appends
-- after the closing paren on the same line.
--
-- When the inner expression is a @case@ (with optional surrounding parens
-- / typecasts) we force-wrap it via 'renderExprWrapped' regardless of its
-- inline length, matching the top-level @case@-always-wraps rule used in
-- 'renderExprDoc' — otherwise a short CASE inside a wrapped parens would
-- collapse onto a single line while its enclosing parens stay multi-line.
--
-- When the inner is a direct @CaseCExpr@ (with no indirection) we emit
-- the compact form @(case ... end)@ — the @case@ keyword on the same
-- line as @(@, and @end)@ together at the close. The @case@/@end@
-- keywords already give the block clear visual structure; opening/closing
-- parens on their own lines are redundant ceremony.
renderInParensWrapped :: Ast.AExpr -> Maybe Ast.Indirection -> Doc ann
renderInParensWrapped expr mInd =
    case (mInd, expr) of
        (Nothing, Ast.CExprAExpr (Ast.CaseCExpr c)) ->
            -- Compact form: `(case ... end)`. We use 'align' to capture
            -- the actual column at @(@ so the case body indents relative
            -- to that column regardless of how the caller positioned us
            -- mid-line (e.g. via explicit @firstPad@ in a SELECT block
            -- list, or via the @"  "@ prefix from 'renderArgRows' inside
            -- a wrapped function call). Body lands at @(-col +
            -- indentSize@; @end@ lines up with @(@; @)@ follows @end@ on
            -- the same line.
            align ("(" <> renderCaseExprWrapped c <> ")")
        _ ->
            let body =
                    if isTopLevelCase expr
                        then renderExprWrapped expr
                        else renderExprMaybeWrap expr
             in "("
                    <> nest indentSize (hardline <> body)
                    <> hardline
                    <> ")"
                    <> maybe mempty renderIndirection mInd


-- | Multi-line @row(...)@ / @(a, b, c)@: same leading-comma block-list
-- as a wrapped function call.
renderExplicitRowWrapped :: Maybe Ast.ExprList -> Doc ann
renderExplicitRowWrapped mExprs =
    let argDocs = maybe [] (map renderExprMaybeWrap . toL) mExprs
     in "row(" <> nest indentSize (renderArgRows argDocs) <> hardline <> ")"


renderImplicitRowWrapped :: Ast.ImplicitRow -> Doc ann
renderImplicitRowWrapped (Ast.ImplicitRow exprs expr) =
    let argDocs = map renderExprMaybeWrap (toL exprs) ++ [renderExprMaybeWrap expr]
     in "(" <> nest indentSize (renderArgRows argDocs) <> hardline <> ")"


-- | Multi-line @case@ expression: each @when ... then ...@ on its own line,
-- @else@ and @end@ on their own lines too. Matches the layout convention
-- used by hand-written SQL.
--
-- Inside a wrapped CASE, any @(select ...)@ subquery in a THEN or ELSE
-- body force-wraps as a multi-line SELECT block (regardless of length)
-- via 'renderExprForceSubqueryWrap'. The convention is that once we've
-- decided the CASE itself reads better multi-line, an inline
-- @(select x from t)@ inside one of its arms looks visually inconsistent
-- next to the multi-line WHEN/ELSE structure — so we expand it too.
renderCaseExprWrapped :: Ast.CaseExpr -> Doc ann
renderCaseExprWrapped (Ast.CaseExpr mArg whenClauses mDefault) =
    "case"
        <> maybe mempty (\arg -> " " <> renderExpr arg) mArg
        <> nest
            indentSize
            ( mconcat [hardline <> renderWhenClauseWrapped w | w <- toL whenClauses]
                <> maybe mempty (\d -> hardline <> "else" <+> renderExprForceSubqueryWrap d) mDefault
            )
        <> hardline
        <> "end"


renderWhenClauseWrapped :: Ast.WhenClause -> Doc ann
renderWhenClauseWrapped (Ast.WhenClause cond result) =
    "when" <+> renderExprMaybeWrap cond <+> "then" <+> renderExprForceSubqueryWrap result


-- | Like 'renderExprMaybeWrap' but force-wraps any directly-applied
-- @(select ...)@ subquery (with optional chained typecast) regardless of
-- inline length. Used inside wrapped CASE arms; falls through to
-- 'renderExprMaybeWrap' for everything that isn't a parenthesized SELECT.
renderExprForceSubqueryWrap :: Ast.AExpr -> Doc ann
renderExprForceSubqueryWrap = \case
    Ast.CExprAExpr (Ast.SelectWithParensCExpr swp mInd) ->
        renderSelectWithParensWrapped swp <> maybe mempty renderIndirection mInd
    Ast.TypecastAExpr inner tn ->
        renderExprForceSubqueryWrap inner <> "::" <> renderTypename tn
    e -> renderExprMaybeWrap e


-- | Multi-line keyword-form pseudo-function. Each constructor knows the
-- syntactic separator between its operands (commas for @coalesce@,
-- @greatest@, @least@; @ as @ for @cast@/@treat@; @ from @ for @extract@,
-- @substring@, @trim@; @ in @ for @position@; @ placing @ /@ from @/@ for @
-- for @overlay@), and uses 'renderArgRows' for the comma-separated cases.
renderFuncExprCommonSubexprWrapped :: Ast.FuncExprCommonSubexpr -> Doc ann
renderFuncExprCommonSubexprWrapped = \case
    Ast.CoalesceFuncExprCommonSubexpr exprs ->
        let argDocs = map renderExprMaybeWrap (toL exprs)
         in "coalesce(" <> nest indentSize (renderArgRows argDocs) <> hardline <> ")"
    Ast.GreatestFuncExprCommonSubexpr exprs ->
        let argDocs = map renderExprMaybeWrap (toL exprs)
         in "greatest(" <> nest indentSize (renderArgRows argDocs) <> hardline <> ")"
    Ast.LeastFuncExprCommonSubexpr exprs ->
        let argDocs = map renderExprMaybeWrap (toL exprs)
         in "least(" <> nest indentSize (renderArgRows argDocs) <> hardline <> ")"
    Ast.NullIfFuncExprCommonSubexpr e1 e2 ->
        let argDocs = [renderExprMaybeWrap e1, renderExprMaybeWrap e2]
         in "nullif(" <> nest indentSize (renderArgRows argDocs) <> hardline <> ")"
    Ast.CastFuncExprCommonSubexpr expr tn ->
        "cast("
            <> nest indentSize (hardline <> renderExprMaybeWrap expr <+> "as" <+> renderTypename tn)
            <> hardline
            <> ")"
    Ast.TreatFuncExprCommonSubexpr expr tn ->
        "treat("
            <> nest indentSize (hardline <> renderExprMaybeWrap expr <+> "as" <+> renderTypename tn)
            <> hardline
            <> ")"
    -- Keyword-form functions whose argument structure isn't a simple
    -- comma-separated list. We render them inline; if length is the
    -- problem we fall back to the inline renderer (which is short
    -- compared to other cases).
    other -> renderFuncExprCommonSubexpr other


-- | Wrap-aware variant for an 'AExpr'. Renders inline first; if the inline
-- form exceeds 'inlineLengthThreshold' AND the expression is structurally
-- wrap-eligible, switches to 'renderExprWrapped'. Pair-shaped sub-calls
-- (nested @jsonb_build_object@-style) force-wrap regardless of length so a
-- wrapped pair-grouped parent doesn't have inline pair-grouped children.
renderExprMaybeWrap :: Ast.AExpr -> Doc ann
renderExprMaybeWrap e =
    let inlineDoc = renderExpr e
        inlineLen = T.length (renderToText inlineDoc)
        forceWrap = hasPairShapedCall e
     in if (inlineLen > inlineLengthThreshold || forceWrap) && isWrapEligibleExpr e
            then renderExprWrapped e
            else inlineDoc


-- | Multi-line render of an @array[...]@ literal: open bracket on the
-- current line, elements laid out with the same leading-comma block-list
-- as 'renderFuncApplicationWrapped', closing bracket back at the caller's
-- column. Each element is rendered through 'renderExprMaybeWrap', so a
-- long inner function call wraps recursively.
renderArrayExprWrapped :: Ast.ArrayExpr -> Doc ann
renderArrayExprWrapped = \case
    Ast.ExprListArrayExpr exprs ->
        let argDocs = map renderExprMaybeWrap (toL exprs)
         in "[" <> nest indentSize (renderArgRows argDocs) <> hardline <> "]"
    Ast.ArrayExprListArrayExpr nested ->
        let argDocs = map (\a -> "array" <> renderArrayExprWrapped a) (toL nested)
         in "[" <> nest indentSize (renderArgRows argDocs) <> hardline <> "]"
    Ast.EmptyArrayExpr -> "[]"


renderFuncAppParams :: Ast.FuncApplicationParams -> Doc ann
renderFuncAppParams = \case
    Ast.NormalFuncApplicationParams mDistinct args mSort ->
        maybe mempty (\b -> if b then "distinct " else "all ") mDistinct
            <> commaJoin (map renderFuncArgExpr (toL args))
            <> maybe mempty (\sc -> " " <> inline render sc) mSort
    Ast.VariadicFuncApplicationParams mArgs varArg mSort ->
        maybe mempty (\args -> commaJoin (map renderFuncArgExpr (toL args)) <> ", ") mArgs
            <> "variadic "
            <> renderFuncArgExpr varArg
            <> maybe mempty (\sc -> " " <> inline render sc) mSort
    Ast.StarFuncApplicationParams -> "*"


renderFuncArgExpr :: Ast.FuncArgExpr -> Doc ann
renderFuncArgExpr = \case
    Ast.ExprFuncArgExpr e -> renderExpr e
    Ast.ColonEqualsFuncArgExpr ident e -> inline render ident <> " := " <> renderExpr e
    Ast.EqualsGreaterFuncArgExpr ident e -> inline render ident <> " => " <> renderExpr e


renderFuncExprCommonSubexpr :: Ast.FuncExprCommonSubexpr -> Doc ann
renderFuncExprCommonSubexpr = \case
    Ast.CoalesceFuncExprCommonSubexpr exprs ->
        "coalesce(" <> commaJoin (map renderExpr (toL exprs)) <> ")"
    Ast.CastFuncExprCommonSubexpr expr tn ->
        "cast(" <> renderExpr expr <+> "as" <+> renderTypename tn <> ")"
    Ast.NullIfFuncExprCommonSubexpr e1 e2 ->
        "nullif(" <> renderExpr e1 <> ", " <> renderExpr e2 <> ")"
    Ast.GreatestFuncExprCommonSubexpr exprs ->
        "greatest(" <> commaJoin (map renderExpr (toL exprs)) <> ")"
    Ast.LeastFuncExprCommonSubexpr exprs ->
        "least(" <> commaJoin (map renderExpr (toL exprs)) <> ")"
    Ast.ExtractFuncExprCommonSubexpr mList ->
        "extract(" <> maybe mempty renderExtractList mList <> ")"
    Ast.TreatFuncExprCommonSubexpr expr tn ->
        "treat(" <> renderExpr expr <+> "as" <+> renderTypename tn <> ")"
    Ast.PositionFuncExprCommonSubexpr mList ->
        "position(" <> maybe mempty renderPositionList mList <> ")"
    Ast.SubstringFuncExprCommonSubexpr mList ->
        "substring(" <> maybe mempty renderSubstrList mList <> ")"
    Ast.TrimFuncExprCommonSubexpr mModifier list ->
        "trim(" <> renderTrimModifier mModifier <> renderTrimList list <> ")"
    Ast.OverlayFuncExprCommonSubexpr list ->
        "overlay(" <> renderOverlayList list <> ")"
    other -> inline render other


renderPositionList :: Ast.PositionList -> Doc ann
renderPositionList (Ast.PositionList needle haystack) =
    inline render needle <+> "in" <+> inline render haystack


renderSubstrList :: Ast.SubstrList -> Doc ann
renderSubstrList = \case
    Ast.ExprListSubstrList exprs -> commaJoin (map renderExpr (toL exprs))
    Ast.ExprSubstrList expr fromFor -> renderExpr expr <+> renderSubstrListFromFor fromFor


renderSubstrListFromFor :: Ast.SubstrListFromFor -> Doc ann
renderSubstrListFromFor = \case
    Ast.FromForSubstrListFromFor e1 e2 ->
        "from" <+> renderExpr e1 <+> "for" <+> renderExpr e2
    Ast.ForFromSubstrListFromFor e1 e2 ->
        "for" <+> renderExpr e1 <+> "from" <+> renderExpr e2
    Ast.FromSubstrListFromFor e -> "from" <+> renderExpr e
    Ast.ForSubstrListFromFor e -> "for" <+> renderExpr e


renderTrimModifier :: Maybe Ast.TrimModifier -> Doc ann
renderTrimModifier = \case
    Nothing -> mempty
    Just Ast.BothTrimModifier -> "both "
    Just Ast.LeadingTrimModifier -> "leading "
    Just Ast.TrailingTrimModifier -> "trailing "


renderTrimList :: Ast.TrimList -> Doc ann
renderTrimList = \case
    Ast.ExprFromExprListTrimList chars from ->
        renderExpr chars <+> "from" <+> commaJoin (map renderExpr (toL from))
    Ast.FromExprListTrimList from ->
        "from" <+> commaJoin (map renderExpr (toL from))
    Ast.ExprListTrimList exprs ->
        commaJoin (map renderExpr (toL exprs))


renderOverlayList :: Ast.OverlayList -> Doc ann
renderOverlayList (Ast.OverlayList target placing fromE mForE) =
    renderExpr target
        <+> "placing"
        <+> renderExpr placing
        <+> "from"
        <+> renderExpr fromE
        <> maybe mempty (\forE -> " for" <+> renderExpr forE) mForE


renderExtractList :: Ast.ExtractList -> Doc ann
renderExtractList (Ast.ExtractList arg expr) =
    renderExtractArg arg <+> "from" <+> renderExpr expr


renderExtractArg :: Ast.ExtractArg -> Doc ann
renderExtractArg = \case
    Ast.IdentExtractArg ident -> renderIdent ident
    Ast.YearExtractArg -> "year"
    Ast.MonthExtractArg -> "month"
    Ast.DayExtractArg -> "day"
    Ast.HourExtractArg -> "hour"
    Ast.MinuteExtractArg -> "minute"
    Ast.SecondExtractArg -> "second"
    Ast.SconstExtractArg (Ast.Sconst s) -> "'" <> pretty s <> "'"


renderBExpr :: Ast.BExpr -> Doc ann
renderBExpr = \case
    Ast.CExprBExpr ce -> renderCExpr ce
    Ast.TypecastBExpr be tn -> renderBExpr be <> "::" <> renderTypename tn
    Ast.PlusBExpr be -> "+" <> renderBExpr be
    Ast.MinusBExpr be -> "-" <> renderBExpr be
    Ast.SymbolicBinOpBExpr left op right ->
        renderBExpr left <+> inline render op <+> renderBExpr right
    other -> inline render other


renderReversableOp :: Bool -> Ast.AExprReversableOp -> Doc ann
renderReversableOp negated = \case
    Ast.BetweenAExprReversableOp symmetric low high ->
        (if negated then "not " else mempty)
            <> (if symmetric then "between symmetric " else "between ")
            <> renderBExpr low
            <+> "and"
            <+> renderExpr high
    Ast.InAExprReversableOp inExpr ->
        (if negated then "not in " else "in ") <> inline render inExpr
    Ast.DistinctFromAExprReversableOp expr ->
        "is " <> (if negated then "not " else mempty) <> "distinct from" <+> renderExpr expr
    other -> inline (aExprReversableOp negated) other


renderTypename :: Ast.Typename -> Doc ann
renderTypename (Ast.Typename _setof stn questionMark mArrayDims) =
    inline render stn
        <> (if questionMark then "?" else mempty)
        <> maybe mempty (\(ad, q) -> inline render ad <> if q then "?" else mempty) mArrayDims


renderTargetEl :: Ast.TargetEl -> Doc ann
renderTargetEl = \case
    Ast.AsteriskTargetEl -> "*"
    Ast.ExprTargetEl e -> renderExpr e
    Ast.AliasedExprTargetEl e alias -> renderExpr e <+> "as" <+> inline render alias
    Ast.ImplicitlyAliasedExprTargetEl e alias -> renderExpr e <+> inline render alias


-- | Wrap-aware target renderer that prefers a multi-line layout when the
-- expression is a top-level function call (optionally with a trailing
-- typecast) and the call has enough arguments to trigger wrapping.
renderTargetElDoc :: Ast.TargetEl -> Doc ann
renderTargetElDoc = \case
    Ast.ExprTargetEl e -> renderExprDoc e
    Ast.AliasedExprTargetEl e alias -> renderExprDoc e <+> "as" <+> inline render alias
    Ast.ImplicitlyAliasedExprTargetEl e alias -> renderExprDoc e <+> inline render alias
    other -> renderTargetEl other


-- | Inline-length threshold used at the top-level select-target / SET-RHS
-- position. Higher than 'inlineLengthThreshold' (which gates wrapping
-- inside a function-arg context) because a select target is allowed to be
-- somewhat longer before wrapping pays off — e.g. @select enqueue(array
-- [mk_payload(...)])@ at ~110 chars reads fine inline, but @select unnest
-- (array[format(...), format(...)])@ at 500+ chars does not.
selectTargetInlineThreshold :: Int
selectTargetInlineThreshold = 150


-- | Wrap-aware expression renderer for SELECT targets and similar
-- top-level positions. Wraps a top-level function application (or a
-- chained-typecast over one) whenever it has enough arguments to trigger
-- the existing arg-count rule. A top-level @case ... end@ (with optional
-- surrounding parens / typecast) always wraps regardless of length —
-- inline @case@ in a SELECT target reads poorly even at moderate widths,
-- and the wrapped form matches the convention of hand-written SQL.
-- Additionally, when the expression's inline form exceeds
-- 'selectTargetInlineThreshold' and the expression is structurally
-- wrap-eligible, it expands multi-line — covers single-arg calls with
-- deeply nested content (e.g. @unnest(array[fn(...), fn(...)])::text@)
-- and other wrap-eligible shapes (parenthesized expressions,
-- @row(...)@, @coalesce@/@cast@, etc.).
renderExprDoc :: Ast.AExpr -> Doc ann
renderExprDoc e = case topLevelFuncApp e of
    Just (app, wrapTypecast)
        | funcAppArgCount app >= funcWrapThreshold ->
            wrapTypecast (renderFuncApplicationWrapped app)
    _
        | isTopLevelCase e ->
            -- 'align' captures the actual column where this expression
            -- starts (which is mid-line after the block-list's
            -- @firstPad@ / @, @ prefix). Subsequent nest-driven indents
            -- inside 'renderExprWrapped' / 'renderCaseExprWrapped' add
            -- to that column, so body @when@ / @else@ land at
            -- @case-col + indentSize@ and @end@ aligns under @case@.
            -- Without this, a bare @case ... end@ has its body indent
            -- driven by the outer nest level, which makes @when@ land
            -- at the same column as @case@ (visually confusing).
            align (renderExprWrapped e)
        | otherwise ->
            let inlineDoc = renderExpr e
                inlineLen = T.length (renderToText inlineDoc)
             in if inlineLen > selectTargetInlineThreshold && isWrapEligibleExpr e
                    then renderExprWrapped e
                    else inlineDoc


-- | True when the expression is a @case ... end@ (with optional surrounding
-- parens and/or chained typecasts). Used in 'renderExprDoc' to force CASE
-- expressions in SELECT-target position to always wrap multi-line.
isTopLevelCase :: Ast.AExpr -> Bool
isTopLevelCase = \case
    Ast.TypecastAExpr inner _ -> isTopLevelCase inner
    Ast.CExprAExpr (Ast.InParensCExpr inner _) -> isTopLevelCase inner
    Ast.CExprAExpr (Ast.CaseCExpr _) -> True
    _ -> False


-- | Wrap-aware table reference renderer used for JOIN right-hand sides.
-- Wraps the call when the operand is a 'FuncTableRef' with a wrap-eligible
-- function call; any other table ref falls back to the inline renderer.
renderTableRefDoc :: Ast.TableRef -> Doc ann
renderTableRefDoc = \case
    Ast.FuncTableRef lp ft mFuncAlias
        | Just (app, ordinality) <- wrapEligibleFuncTable ft ->
            (if lp then "lateral " else mempty)
                <> renderFuncApplicationWrapped app
                <> (if ordinality then " with ordinality" else mempty)
                <> maybe mempty (\a -> " " <> renderFuncAliasClauseDoc a) mFuncAlias
    Ast.SelectTableRef lp swp mAlias ->
        (if lp then "lateral " else mempty)
            <> renderSelectWithParensMaybeWrap swp
            <> maybe mempty (\a -> " " <> inline render a) mAlias
    other -> renderTableRefInline other


-- | Render a function-alias clause (the @as alias_name (col1, col2, ...)@
-- after a FROM-list / JOIN function call). Short column lists stay inline;
-- lists of more than 3 columns wrap onto their own bracket-list block
-- below the alias name.
renderFuncAliasClauseDoc :: Ast.FuncAliasClause -> Doc ann
renderFuncAliasClauseDoc = \case
    Ast.AliasFuncAliasClause (Ast.AliasClause hasAs name (Just cols))
        | length (toL cols) > 3 ->
            let prefix = if hasAs then "as " else mempty
                colDocs = map (inline render) (toL cols)
             in prefix
                    <> inline render name
                    <> hardline
                    <> indent indentSize (renderBracketList colDocs)
    other -> inline render other


-- | If the FuncTable wraps a single ApplicationFuncExprWindowless with enough
-- arguments to trigger wrapping, return the application and its WITH ORDINALITY
-- flag; otherwise Nothing.
wrapEligibleFuncTable :: Ast.FuncTable -> Maybe (Ast.FuncApplication, Bool)
wrapEligibleFuncTable = \case
    Ast.FuncExprFuncTable (Ast.ApplicationFuncExprWindowless app) (Ast.OptOrdinality ordinality)
        | funcAppArgCount app >= funcWrapThreshold
        , T.length (renderToText (renderFuncApplication app)) > inlineLengthThreshold ->
            Just (app, ordinality)
    _ -> Nothing


renderTargetListInline :: Ast.TargetList -> Doc ann
renderTargetListInline = commaJoin . map renderTargetEl . toL


renderSetClause :: Ast.SetClause -> Doc ann
renderSetClause = \case
    Ast.TargetSetClause target expr ->
        inline render target
            <+> "="
            <+> nest indentSize (renderExprMaybeWrap expr)
    Ast.TargetListSetClause targets expr ->
        "("
            <> commaJoin (map (inline render) (toL targets))
            <> ") ="
            <+> nest indentSize (renderExprMaybeWrap expr)


renderInsertTarget :: Ast.InsertTarget -> Doc ann
renderInsertTarget (Ast.InsertTarget name mAlias) =
    inline render name <> maybe mempty (\a -> " as " <> inline render a) mAlias


-- Inline renderers for table refs / function tables — used inside FROM lists
-- that fit on a single line, and inside joined-table renderings where the
-- right-hand side is always inline.

renderTableRefInline :: Ast.TableRef -> Doc ann
renderTableRefInline = \case
    Ast.RelationExprTableRef re mAlias mSample ->
        let base = inline render re
            aliased = maybe base (\a -> base <+> inline render a) mAlias
         in maybe aliased (\s -> aliased <+> inline render s) mSample
    Ast.FuncTableRef lp ft mFuncAlias ->
        (if lp then "lateral " else mempty) <> renderFuncTable ft <> maybe mempty (\a -> " " <> renderFuncAliasClauseDoc a) mFuncAlias
    Ast.SelectTableRef lp swp mAlias ->
        (if lp then "lateral " else mempty) <> renderSelectWithParensInline swp <> maybe mempty (\a -> " " <> inline render a) mAlias
    Ast.JoinTableRef jt mAlias ->
        inline render jt <> maybe mempty (\a -> " " <> inline render a) mAlias


renderFuncTable :: Ast.FuncTable -> Doc ann
renderFuncTable = \case
    Ast.FuncExprFuncTable few (Ast.OptOrdinality ordinality) ->
        renderFuncExprWindowless few <> if ordinality then " with ordinality" else mempty
    Ast.RowsFromFuncTable rowsList (Ast.OptOrdinality ordinality) ->
        "rows from (" <> commaJoin (map renderRowsfromItem (toL rowsList)) <> ")" <> if ordinality then " with ordinality" else mempty


renderFuncExprWindowless :: Ast.FuncExprWindowless -> Doc ann
renderFuncExprWindowless = \case
    Ast.ApplicationFuncExprWindowless app -> renderFuncApplication app
    Ast.CommonSubexprFuncExprWindowless sub -> renderFuncExprCommonSubexpr sub


renderRowsfromItem :: Ast.RowsfromItem -> Doc ann
renderRowsfromItem (Ast.RowsfromItem few mColDef) =
    renderFuncExprWindowless few <> maybe mempty (\cd -> " as (" <> inline render cd <> ")") mColDef


-- Inline select renderers for subqueries that appear on a single line
-- (e.g. inside `EXISTS (...)`, `= (select ...)`, etc.).

renderSelectWithParensInline :: Ast.SelectWithParens -> Doc ann
renderSelectWithParensInline = \case
    Ast.NoParensSelectWithParens snp -> "(" <> renderSelectNoParensInline snp <> ")"
    Ast.WithParensSelectWithParens swp -> "(" <> renderSelectWithParensInline swp <> ")"


-- | Inline-length threshold for in-expression parenthesized SELECTs. Higher
-- than 'inlineLengthThreshold' (which gates function-argument wrapping)
-- because subqueries are naturally wider than a function arg, so the
-- threshold needs to admit shapes like @(select 1::int from gen(...))@
-- without forcing them onto multiple lines.
subqueryInlineThreshold :: Int
subqueryInlineThreshold = 100


-- | Render an in-expression parenthesized SELECT (used for @EXISTS (...)@,
-- @ARRAY (...)@, @UNIQUE (...)@, scalar subselects, and @ANY/ALL (subquery)@)
-- inline when its inline form fits within 'subqueryInlineThreshold',
-- otherwise expand it onto multiple lines so the inner query reads as a
-- normal block (FROM split off, WHERE on its own line with right-aligned
-- AND continuations, leading-comma SELECT lists, etc. — the same layout
-- principles a top-level SELECT uses).
renderSelectWithParensMaybeWrap :: Ast.SelectWithParens -> Doc ann
renderSelectWithParensMaybeWrap swp =
    let inlineDoc = renderSelectWithParensInline swp
        inlineLen = T.length (renderToText inlineDoc)
     in if inlineLen > subqueryInlineThreshold
            then renderSelectWithParensWrapped swp
            else inlineDoc


-- | Multi-line render of a parenthesized SELECT: open paren on the current
-- line, inner statement on its own block one indent deeper, closing paren
-- back at the caller's column. Mirrors the layout used by
-- 'renderCte' / @WITH ... AS (...)@.
renderSelectWithParensWrapped :: Ast.SelectWithParens -> Doc ann
renderSelectWithParensWrapped = \case
    Ast.NoParensSelectWithParens snp ->
        "(" <> nest indentSize (hardline <> renderSelectNoParens snp) <> hardline <> ")"
    Ast.WithParensSelectWithParens inner ->
        "(" <> nest indentSize (hardline <> renderSelectWithParensWrapped inner) <> hardline <> ")"


renderSelectNoParensInline :: Ast.SelectNoParens -> Doc ann
renderSelectNoParensInline (Ast.SelectNoParens mWith selectClause mSort mLimit mLocking) =
    spaceJoin $
        catMaybes
            [ inline render <$> mWith
            , Just (renderSelectClauseInline selectClause)
            , inline render <$> mSort
            , inline render <$> mLimit
            , inline render <$> mLocking
            ]


-- | Concatenate inline 'Doc's with a single space between each. Used wherever
-- the previous Text-based renderer used 'T.unwords'.
spaceJoin :: [Doc ann] -> Doc ann
spaceJoin = hcat . punctuate " "


renderSelectClauseInline :: Ast.SelectClause -> Doc ann
renderSelectClauseInline = \case
    Ast.SimpleSelectSelectClause ss -> renderSimpleSelectInline ss
    Ast.WithParensSelectClause swp -> renderSelectWithParensInline swp


renderSimpleSelectInline :: Ast.SimpleSelect -> Doc ann
renderSimpleSelectInline = \case
    Ast.NormalSimpleSelect mTargeting mInto mFrom mWhere mGroup mHaving mWindow ->
        spaceJoin $
            catMaybes
                [ Just (renderTargetingInline mTargeting)
                , inline render <$> mInto
                , renderFromClauseInline <$> mFrom
                , (\(Ast.WhereClause w) -> "where" <+> renderExpr w) <$> mWhere
                , inline render <$> mGroup
                , (\(Ast.HavingClause h) -> "having" <+> renderExpr h) <$> mHaving
                , inline render <$> mWindow
                ]
    Ast.BinSimpleSelect op left mAll right ->
        renderSelectClauseInline left
            <+> inline render op
            <> maybe mempty (\distinct -> if distinct then " distinct" else " all") mAll
            <+> renderSelectClauseInline right
    other -> inline render other


renderFromClauseInline :: Ast.FromClause -> Doc ann
renderFromClauseInline fromList =
    "from " <> commaJoin (map renderTableRefInline (toL fromList))


renderTargetingInline :: Maybe Ast.Targeting -> Doc ann
renderTargetingInline = \case
    Nothing -> "select"
#ifdef KRONOR_SYNTAX
    Just (Ast.NormalTargeting tl) -> "select " <> commaJoin (map renderHsTargetElInline (toL tl))
#else
    Just (Ast.NormalTargeting tl) -> "select " <> commaJoin (map renderTargetEl (toL tl))
#endif
    Just (Ast.AllTargeting Nothing) -> "select all"
    Just (Ast.AllTargeting (Just tl)) -> "select all " <> commaJoin (map renderTargetEl (toL tl))
    Just (Ast.DistinctTargeting mExprs tl) ->
        let prefix = case mExprs of
                Nothing -> "select distinct"
                Just exprs -> "select distinct on (" <> inline render exprs <> ")"
         in prefix <> " " <> commaJoin (map renderTargetEl (toL tl))


#ifdef KRONOR_SYNTAX

-- | Render a hasql-th target element on a single line. Used in inline subquery
-- contexts. The multi-line `$Name{...}` layout is handled separately by
-- 'renderHsTargetEl'.
renderHsTargetElInline :: Ast.HsTargetEl -> Doc ann
renderHsTargetElInline = \case
    Ast.HsFuncTargetEl name args ->
        "$" <> pretty name <> " (" <> commaJoin (map renderHsTargetElInline args) <> ")"
    Ast.HsRecTargetEl name fields ->
        "$"
            <> pretty name
            <> "{ "
            <> commaJoin (map renderHsFieldContentInline (toL fields))
            <> " }"
    Ast.SqlTargetEl te -> renderTargetEl te


renderHsFieldContentInline :: Ast.HsFieldTargetEl -> Doc ann
renderHsFieldContentInline (Ast.HsFieldEl name target) =
    pretty name <> " = " <> renderHsTargetElInline target

#endif


----------------------------------------------------------------------
-- Multi-line statement / clause renderers (return Doc).
----------------------------------------------------------------------

renderPreparableStmt :: Ast.PreparableStmt -> Doc ann
renderPreparableStmt = \case
    Ast.SelectPreparableStmt s -> renderSelectStmt s
    Ast.InsertPreparableStmt s -> renderInsertStmt s
    Ast.UpdatePreparableStmt s -> renderUpdateStmt s
    Ast.DeletePreparableStmt s -> renderDeleteStmt s
    Ast.CallPreparableStmt s -> inline render s


renderSelectStmt :: Ast.SelectStmt -> Doc ann
renderSelectStmt = \case
    Ast.NoParensSelectStmt snp -> renderSelectNoParens snp
    Ast.WithParensSelectStmt swp -> renderSelectWithParens swp


renderSelectWithParens :: Ast.SelectWithParens -> Doc ann
renderSelectWithParens = \case
    Ast.NoParensSelectWithParens snp -> "(" <> renderSelectNoParens snp <> ")"
    Ast.WithParensSelectWithParens swp -> "(" <> renderSelectWithParens swp <> ")"


renderSelectNoParens :: Ast.SelectNoParens -> Doc ann
renderSelectNoParens (Ast.SelectNoParens mWith selectClause mSort mLimit mLocking) =
    vsep $
        catMaybes
            [ renderWithClause <$> mWith
            , Just (renderSelectClause selectClause)
            , inline render <$> mSort
            , renderSelectLimit <$> mLimit
            , inline render <$> mLocking
            ]


renderSelectClause :: Ast.SelectClause -> Doc ann
renderSelectClause = \case
    Ast.SimpleSelectSelectClause ss -> renderSimpleSelect ss
    Ast.WithParensSelectClause swp -> renderSelectWithParens swp


renderSimpleSelect :: Ast.SimpleSelect -> Doc ann
renderSimpleSelect = \case
    Ast.NormalSimpleSelect mTargeting mInto mFrom mWhere mGroup mHaving mWindow ->
        vsep $
            catMaybes
                [ Just (renderTargeting mTargeting)
                , inline render <$> mInto
                , renderFromClause <$> mFrom
                , renderWhereClause <$> mWhere
                , inline render <$> mGroup
                , (\(Ast.HavingClause h) -> renderCondWithAndAligned (T.length "having") "having" h) <$> mHaving
                , inline render <$> mWindow
                ]
    Ast.ValuesSimpleSelect vc ->
        renderValuesClause vc
    Ast.TableSimpleSelect re ->
        "table" <+> inline render re
    Ast.BinSimpleSelect op left mAll right ->
        vsep
            [ renderSelectClause left
            , inline render op <> maybe mempty (\d -> if d then " distinct" else " all") mAll
            , renderSelectClause right
            ]


-- | Render the SELECT targeting (column list).
renderTargeting :: Maybe Ast.Targeting -> Doc ann
renderTargeting = \case
    Nothing -> "select"
#ifdef KRONOR_SYNTAX
    Just (Ast.NormalTargeting tl) -> renderHsTargetList "select" tl
#else
    Just (Ast.NormalTargeting tl) -> renderSqlTargetList "select" tl
#endif
    Just (Ast.AllTargeting Nothing) -> "select all"
    Just (Ast.AllTargeting (Just tl)) -> renderSqlTargetList "select all" tl
    Just (Ast.DistinctTargeting mExprs tl) ->
        let prefix = case mExprs of
                Nothing -> "select distinct"
                Just exprs -> "select distinct on (" <> inline render exprs <> ")"
         in renderSqlTargetList prefix tl


#ifdef KRONOR_SYNTAX

-- | Block-list layout for hasql-th target lists. First item lands at one
-- indent past the SELECT keyword; subsequent items are emitted as `, item` at
-- (indent - 2), so every item's content column lines up vertically.
--
-- A single target that's a top-level @case ... end@ (with optional
-- surrounding parens / typecasts) forces the block-list layout too, so
-- @case@ lands at the item-content column rather than mid-line after
-- @select @. Otherwise the @align@ inside 'renderExprDoc' would capture
-- the @case@ keyword's mid-line column and push the body / @end@ far to
-- the right.
renderHsTargetList :: Doc ann -> Ast.HsTargetList -> Doc ann
renderHsTargetList prefix tl =
    let items = toL tl
        hasRecord = any isRecTarget items
     in case items of
            [single]
                | not hasRecord && not (singleHsItemForcesMultiLine single) ->
                    prefix <+> renderHsTargetEl single
            _ -> renderBlockListWith prefix renderHsTargetEl items
  where
    isRecTarget (Ast.HsRecTargetEl _ _) = True
    isRecTarget _ = False

#endif


renderSqlTargetList :: Doc ann -> Ast.TargetList -> Doc ann
renderSqlTargetList prefix tl =
    let items = toL tl
     in case items of
            [single]
                | not (singleSqlItemForcesMultiLine single) ->
                    prefix <+> renderTargetElDoc single
            _ -> renderBlockListWith prefix renderTargetElDoc items


-- | A single SELECT target item forces block-list layout when its
-- expression would render multi-line via 'renderExprDoc'. Currently this
-- is just the top-level CASE rule (peeling typecasts and parens) — keeping
-- the predicate narrow avoids unintended layout changes for short single
-- targets.
#ifdef KRONOR_SYNTAX
singleHsItemForcesMultiLine :: Ast.HsTargetEl -> Bool
singleHsItemForcesMultiLine = \case
    Ast.SqlTargetEl te -> singleSqlItemForcesMultiLine te
    _ -> False
#endif


singleSqlItemForcesMultiLine :: Ast.TargetEl -> Bool
singleSqlItemForcesMultiLine = \case
    Ast.ExprTargetEl e -> isTopLevelCase e
    Ast.AliasedExprTargetEl e _ -> isTopLevelCase e
    Ast.ImplicitlyAliasedExprTargetEl e _ -> isTopLevelCase e
    _ -> False


-- | Build the block-list layout used by SELECT target lists and the
-- multi-assignment UPDATE SET. Pattern:
--
-- @
-- prefix
--     first
--   , second
--   , third
-- @
--
-- The body sits at indent 4; commas at indent 2.
--
-- Note: padding is applied as explicit spaces in the Doc rather than via
-- 'indent'/'nest'. This is deliberate — we want the *first* item one indent
-- deeper than the comma rows, so that the comma column lines up under the
-- prefix while item content aligns. 'indent' would push every row to the same
-- column.
renderBlockListWith :: Doc ann -> (a -> Doc ann) -> [a] -> Doc ann
renderBlockListWith prefix renderItem items =
    prefix
        <> hardline
        <> vsep (zipWith renderRow [(0 :: Int) ..] items)
  where
    firstPad = pretty (T.replicate indentSize " ")
    commaPad = pretty (T.replicate (max 0 (indentSize - 2)) " ")
    renderRow 0 item = firstPad <> renderItem item
    renderRow _ item = commaPad <> ", " <> renderItem item


#ifdef KRONOR_SYNTAX

-- | Multi-line `$Name{...}` record layout:
--
-- @

-- $Name
--     { field1 = ...
--     , field2 = ...
--     }
-- @
--
-- Wrapped in 'align' so any hard line break within the record body picks up
-- the column where `$Name` was placed (rather than the outer block's column);
-- 'indent' then takes us one level deeper for the brace/fields/closing brace.


renderHsTargetEl :: Ast.HsTargetEl -> Doc ann
renderHsTargetEl = \case
    Ast.HsFuncTargetEl name args ->
        let inlineDoc = "$" <> pretty name <> " (" <> commaJoin (map renderHsTargetElInline args) <> ")"
            inlineLen = T.length (renderToText inlineDoc)
         in if not (null args) && inlineLen > selectTargetInlineThreshold
                then renderHsFuncTargetWrapped name args
                else inlineDoc
    Ast.HsRecTargetEl name fields ->
        align $
            "$"
                <> pretty name
                <> hardline
                <> indent indentSize (renderRecordFields fields)
    Ast.SqlTargetEl te -> renderTargetElDoc te


-- | Multi-line layout for hasql-th @$Name (...)@: each arg on its own
-- line in a leading-comma block, same shape as
-- 'renderFuncApplicationWrapped'. Each arg is rendered wrap-aware so a
-- long inner SQL expression (CASE, function call, parenthesized cast,
-- etc.) wraps the same way it would at the top-level SELECT-target
-- position. Used both for top-level @$Foo (...)@ targets (gated by
-- 'selectTargetInlineThreshold') and for nested @$Foo (...)@ values
-- inside record fields (gated by 'inlineLengthThreshold' via
-- 'renderHsTargetElMaybeWrap').
renderHsFuncTargetWrapped :: Text -> [Ast.HsTargetEl] -> Doc ann
renderHsFuncTargetWrapped name args =
    "$"
        <> pretty name
        <> " ("
        <> nest indentSize (renderArgRows (map renderHsFuncTargetArg args))
        <> hardline
        <> ")"


-- | Render one arg of a wrapped @$Name (...)@. SQL args go through
-- 'renderTargetElDoc' (the wrap-aware SELECT-target renderer); nested
-- @$Foo (...)@ / @$Foo{...}@ args recurse via 'renderHsTargetElMaybeWrap'
-- so a deeply nested splice picks up the same inline-vs-wrap rule it
-- would in a record field.
renderHsFuncTargetArg :: Ast.HsTargetEl -> Doc ann
renderHsFuncTargetArg = \case
    Ast.SqlTargetEl te -> renderTargetElDoc te
    other -> renderHsTargetElMaybeWrap other


-- | Same as 'renderHsTargetEl' but without the outer 'align' — for use
-- when this target is a nested record value (rendered inside
-- 'renderRecordFields'). We rely on the surrounding @indent indentSize@
-- to give us the right base column, and propagate it via plain
-- @hardline + indent indentSize@ so each nesting level adds exactly
-- @indentSize@ of indent rather than capturing the field's mid-line
-- column (which would cascade rightward).
renderHsTargetElWrappedInner :: Ast.HsTargetEl -> Doc ann
renderHsTargetElWrappedInner = \case
    Ast.HsRecTargetEl name fields ->
        "$"
            <> pretty name
            <> hardline
            <> indent indentSize (renderRecordFields fields)
    Ast.HsFuncTargetEl name args ->
        renderHsFuncTargetWrapped name args
    other -> renderHsTargetEl other


renderRecordFields :: NE.NonEmpty Ast.HsFieldTargetEl -> Doc ann
renderRecordFields fields =
    let firstField = NE.head fields
        restFields = NE.tail fields
        renderField (Ast.HsFieldEl name target) =
            pretty name <> " = " <> renderHsTargetElMaybeWrap target
        firstLine = "{ " <> renderField firstField
        restLines = map (\f -> ", " <> renderField f) restFields
     in vsep $ firstLine : restLines ++ ["}"]


-- | Wrap-aware renderer for hasql-th @$Foo@ / @$Foo{...}@ target
-- expressions used inside record fields. Renders inline first; if the
-- inline form exceeds 'inlineLengthThreshold' AND the target is a
-- record-shaped @$Foo{...}@ (or a function-shaped @$f(...)@ that
-- contains a wrap-eligible record), expands to multi-line via
-- 'renderHsTargetElWrappedInner' so each nested level adds exactly
-- @indentSize@ of indent rather than cascading rightward.
renderHsTargetElMaybeWrap :: Ast.HsTargetEl -> Doc ann
renderHsTargetElMaybeWrap el =
    let inlineDoc = renderHsTargetElInline el
        inlineLen = T.length (renderToText inlineDoc)
     in if inlineLen > inlineLengthThreshold && isHsTargetWrapEligible el
            then renderHsTargetElWrappedInner el
            else inlineDoc


isHsTargetWrapEligible :: Ast.HsTargetEl -> Bool
isHsTargetWrapEligible = \case
    Ast.HsRecTargetEl _ _ -> True
    Ast.HsFuncTargetEl _ args -> not (null args)
    Ast.SqlTargetEl _ -> False

#endif


-- | Render the FROM clause. A list with no joins stays on a single line
-- (`from t1, t2`). Any join forces multi-line layout so each join lands on its
-- own line with its ON/USING qualification below.
renderFromClause :: Ast.FromClause -> Doc ann
renderFromClause fromList =
    let items = toL fromList
        lastIdx = length items - 1
     in if any forcesMultiLine items
            then
                vsep $
                    zipWith
                        ( \i item ->
                            renderTableRef (i == (0 :: Int)) item
                                <> if i < lastIdx then "," else mempty
                        )
                        [0 ..]
                        items
            else "from" <+> commaJoin (map renderTableRefInline items)
  where
    -- A FROM item forces multi-line layout when it's a JOIN (each arm on
    -- its own line), a wrap-eligible function call (e.g.
    -- @from job_queue.enqueue_payload(array[...]::jsonb[], priority_ :=
    -- 5000)@), or a parenthesized SELECT whose inline form exceeds
    -- 'subqueryInlineThreshold'.
    forcesMultiLine (Ast.JoinTableRef _ _) = True
    forcesMultiLine (Ast.FuncTableRef _ ft _) =
        case wrapEligibleFuncTable ft of
            Just _ -> True
            Nothing -> False
    forcesMultiLine (Ast.SelectTableRef _ swp _) =
        T.length (renderToText (renderSelectWithParensInline swp)) > subqueryInlineThreshold
    forcesMultiLine _ = False


renderTableRef :: Bool -> Ast.TableRef -> Doc ann
renderTableRef isFirst = \case
    Ast.RelationExprTableRef re mAlias mSample ->
        let base = inline render re
            aliased = maybe base (\a -> base <> " " <> inline render a) mAlias
            sampled = maybe aliased (\s -> aliased <> " " <> inline render s) mSample
         in pfx <> sampled
    Ast.FuncTableRef lp ft mFuncAlias
        | Just (app, ordinality) <- wrapEligibleFuncTable ft ->
            pfx
                <> (if lp then "lateral " else mempty)
                <> renderFuncApplicationWrapped app
                <> (if ordinality then " with ordinality" else mempty)
                <> maybe mempty (\a -> " " <> renderFuncAliasClauseDoc a) mFuncAlias
    Ast.FuncTableRef lp ft mFuncAlias ->
        pfx
            <> ( (if lp then "lateral " else mempty)
                    <> renderFuncTable ft
                    <> maybe mempty (\a -> " " <> renderFuncAliasClauseDoc a) mFuncAlias
               )
    Ast.SelectTableRef lp swp mAlias ->
        pfx
            <> (if lp then "lateral " else mempty)
            <> renderSelectWithParensMaybeWrap swp
            <> maybe mempty (\a -> " " <> inline render a) mAlias
    Ast.JoinTableRef jt mAlias ->
        pfx <> renderJoinedTable jt <> maybe mempty (\a -> " " <> inline render a) mAlias
  where
    pfx = if isFirst then "from " else mempty


-- | Render a joined table. The @from @ prefix is the caller's responsibility
-- (added by 'renderTableRef' when the surrounding 'JoinTableRef' is the first
-- FROM item) so that parenthesized or non-first joins don't end up with the
-- @from@ keyword inside the parens or duplicated mid-list.
renderJoinedTable :: Ast.JoinedTable -> Doc ann
renderJoinedTable = \case
    Ast.InParensJoinedTable jt -> "(" <> renderJoinedTable jt <> ")"
    Ast.CrossJoinedTable left right ->
        renderTableRef False left <> hardline <> ("cross join" <+> renderTableRefDoc right)
    Ast.NaturalJoinedTable left mType right ->
        let prefix = "natural " <> maybe "" (\t -> renderJoinType t <> " ") mType <> "join"
         in renderTableRef False left <> hardline <> (pretty prefix <+> renderTableRefDoc right)
    Ast.QualJoinedTable left mType right qual ->
        -- Right-align the ON keyword (and any AND continuations) under the
        -- first word of the JOIN-block (`join` itself for a plain join, or
        -- `left` / `right` / `inner` / `full` for a prefixed one). This keeps
        -- the qualifier visually attached to the join opener regardless of
        -- how long the full keyword sequence is.
        let mTypePrefix = maybe "" (\t -> renderJoinType t <> " ") mType
            keyword = mTypePrefix <> "join"
            anchor = firstWordLen keyword
            joinHeader = pretty keyword <+> renderTableRefDoc right
         in renderTableRef False left <> hardline <> joinHeader <> hardline <> renderJoinQual anchor qual


-- | Length of the first whitespace-delimited word in @t@ (or 0 for empty).
firstWordLen :: Text -> Int
firstWordLen = T.length . T.takeWhile (/= ' ')


renderJoinType :: Ast.JoinType -> Text
renderJoinType = \case
    Ast.FullJoinType outer -> "full" <> if outer then " outer" else ""
    Ast.LeftJoinType outer -> "left" <> if outer then " outer" else ""
    Ast.RightJoinType outer -> "right" <> if outer then " outer" else ""
    Ast.InnerJoinType -> "inner"


renderJoinQual :: Int -> Ast.JoinQual -> Doc ann
renderJoinQual anchor = \case
    Ast.OnJoinQual expr -> renderCondWithAndAligned anchor "on" expr
    Ast.UsingJoinQual cols ->
        "using (" <> commaJoin (map (inline render) (toL cols)) <> ")"


-- | Render WHERE clause; AND continuations right-align with `where`'s end.
renderWhereClause :: Ast.WhereClause -> Doc ann
renderWhereClause (Ast.WhereClause expr) = renderCondWithAndAligned (T.length "where") "where" expr


-- | Render a condition expression. AND continuations are emitted with explicit
-- left-padding so their last letter lines up with the @anchor@-wide opening
-- keyword (e.g. @anchor = 5@ for `where`, @anchor = 4@ for `join`-block ON).
--
-- AND splitting works on the rendered Text rather than the AST because
-- @postgresql-syntax@ parses @a = 1 AND b = 2@ as
-- @SymbolicBinOpAExpr a (=) (AndAExpr 1 (SymbolicBinOpAExpr b (=) ...))@ —
-- the AND ends up nested inside the comparison rather than at the top level.
renderCondWithAndAligned :: Int -> Text -> Ast.AExpr -> Doc ann
renderCondWithAndAligned anchor keyword expr =
    let renderedText = renderToText (renderExpr expr)
        parts = splitOnAnd renderedText
        kwPadLen = max 0 (anchor - T.length keyword)
        andPadLen = max 0 (anchor - T.length "and")
        kwPad = pretty (T.replicate kwPadLen " ")
        andPad = pretty (T.replicate andPadLen " ")
     in case parts of
            -- Single condition (no top-level AND): if its inline form is
            -- long and the expression is wrap-eligible, hand it off to
            -- 'renderExprMaybeWrap' so the inner shape (e.g. a long
            -- function call or @case@) can expand multi-line.
            [single]
                | T.length single > inlineLengthThreshold && isWrapEligibleExpr expr ->
                    kwPad <> pretty keyword <+> renderExprMaybeWrap expr
                | otherwise ->
                    kwPad <> pretty keyword <+> pretty single
            (first : rest) ->
                vsep $
                    (kwPad <> pretty keyword <+> pretty first)
                        : map (\p -> andPad <> "and" <+> pretty p) rest
            [] -> mempty


-- | Render an inline 'Doc' down to 'Text'. Used in the few places we need
-- to do text-level operations (e.g. AND splitting) on the rendered output
-- before re-wrapping into a 'Doc'.
renderToText :: Doc ann -> Text
renderToText = renderStrict . layoutPretty layoutOpts


-- | Split rendered SQL text at top-level @ and @ boundaries. Skips
-- occurrences inside parens, under a @between@, and inside single-quoted
-- string literals or double-quoted identifiers (@'@ / @"@, with standard
-- doubled-quote escapes).
splitOnAnd :: Text -> [Text]
splitOnAnd = go 0 False Nothing "" . T.unpack
  where
    go :: Int -> Bool -> Maybe Char -> String -> String -> [Text]
    go _ _ _ acc [] = [T.strip (T.pack (reverse acc))]
    -- Inside a string/quoted-identifier: doubled-quote escape stays inside;
    -- otherwise the real closing quote exits.
    go d btwn (Just q) acc (c1 : c2 : rest)
        | c1 == q && c2 == q = go d btwn (Just q) (c2 : c1 : acc) rest
    go d btwn (Just q) acc (c : rest)
        | c == q = go d btwn Nothing (c : acc) rest
        | otherwise = go d btwn (Just q) (c : acc) rest
    -- Outside strings:
    go d btwn Nothing acc ('\'' : rest) = go d btwn (Just '\'') ('\'' : acc) rest
    go d btwn Nothing acc ('"' : rest) = go d btwn (Just '"') ('"' : acc) rest
    go d btwn Nothing acc ('(' : rest) = go (d + 1) btwn Nothing ('(' : acc) rest
    go d btwn Nothing acc (')' : rest) = go (max 0 (d - 1)) btwn Nothing (')' : acc) rest
    go 0 True Nothing acc (' ' : 'a' : 'n' : 'd' : ' ' : rest) =
        go 0 False Nothing (reverse " and " ++ acc) rest
    go 0 _ Nothing acc (' ' : 'a' : 'n' : 'd' : ' ' : rest) =
        T.strip (T.pack (reverse acc)) : go 0 False Nothing "" rest
    go d _ Nothing acc ('b' : 'e' : 't' : 'w' : 'e' : 'e' : 'n' : ' ' : rest) =
        go d True Nothing (reverse "between " ++ acc) rest
    go d btwn st acc (c : rest) = go d btwn st (c : acc) rest


renderSelectLimit :: Ast.SelectLimit -> Doc ann
renderSelectLimit = \case
    Ast.LimitOffsetSelectLimit lc oc ->
        inline render lc <> hardline <> inline render oc
    Ast.OffsetLimitSelectLimit oc lc ->
        inline render oc <> hardline <> inline render lc
    Ast.LimitSelectLimit lc -> inline render lc
    Ast.OffsetSelectLimit oc -> inline render oc


renderWithClause :: Ast.WithClause -> Doc ann
renderWithClause (Ast.WithClause recursive ctes) =
    let keyword :: Text
        keyword = if recursive then "with recursive" else "with"
        cteList = toL ctes
        lastIdx = length cteList - 1
     in vsep $
            zipWith
                ( \i cte ->
                    let isLast = i == lastIdx
                        prefix = if i == (0 :: Int) then pretty keyword <> " " else mempty
                     in prefix <> renderCte isLast cte
                )
                [0 :: Int ..]
                cteList


renderCte :: Bool -> Ast.CommonTableExpr -> Doc ann
renderCte isLast (Ast.CommonTableExpr name mCols mMaterialized stmt) =
    let suffix = if isLast then ")" else "),"
        materializedKeyword = case mMaterialized of
            Nothing -> ""
            Just True -> "materialized "
            Just False -> "not materialized "
        header =
            inline render name
                <> maybe
                    mempty
                    (\cols -> " (" <> commaJoin (map (inline render) (toL cols)) <> ")")
                    mCols
                <> " as "
                <> materializedKeyword
                <> "("
     in header
            <> hardline
            <> indent indentSize (renderPreparableStmt stmt)
            <> hardline
            <> suffix


-- | Render INSERT statement.
renderInsertStmt :: Ast.InsertStmt -> Doc ann
renderInsertStmt (Ast.InsertStmt mWith target rest mConflict mReturning) =
    vsep $
        catMaybes
            [ renderWithClause <$> mWith
            , Just ("insert into" <+> renderInsertTarget target)
            , Just (renderInsertRest rest)
            , renderOnConflict <$> mConflict
            , (\(Ast.ReturningClause rc) -> "returning" <+> renderTargetListInline rc) <$> mReturning
            ]


renderInsertRest :: Ast.InsertRest -> Doc ann
renderInsertRest = \case
    Ast.SelectInsertRest mCols mOverride selectStmt ->
        let colsDoc = maybe mempty renderInsertColumnList mCols
            overrideDoc = case mOverride of
                Nothing -> mempty
                Just o -> "overriding " <> inline render o <> " value" <> hardline
            selectDoc = overrideDoc <> renderSelectStmt selectStmt
         in case mCols of
                Nothing -> selectDoc
                Just _ -> colsDoc <> hardline <> selectDoc
    Ast.DefaultValuesInsertRest -> "default values"


-- | Render the bracketed INSERT column list. Short lists (≤ 3 cols) stay on
-- one line wrapped in `(...)`; longer lists use the bracket-list block-comma
-- layout.
renderInsertColumnList :: Ast.InsertColumnList -> Doc ann
renderInsertColumnList cols =
    let items = toL cols
        rendered = map (inline render) items
     in if length items <= 3
            then indent indentSize ("(" <> commaJoin rendered <> ")")
            else indent indentSize (renderBracketList rendered)


-- | Build the bracket-list layout used for long INSERT column lists and
-- VALUES tuples:
--
-- @
-- ( first
-- , second
-- , third
-- )
-- @
renderBracketList :: [Doc ann] -> Doc ann
renderBracketList = \case
    [] -> "()"
    (x : xs) ->
        vsep $
            ("( " <> x)
                : map (", " <>) xs
                ++ [")"]


-- | Render VALUES clause. The keyword `values` lands on its own line with the
-- tuples emitted as bracket-lists below it.
renderValuesClause :: Ast.ValuesClause -> Doc ann
renderValuesClause vc =
    let rows = toL vc
        lastIdx = length rows - 1
        renderRow i row =
            let bracketed = renderValuesRow row
             in if i < lastIdx then bracketed <> "," else bracketed
     in "values"
            <> hardline
            <> indent indentSize (vsep (zipWith renderRow [(0 :: Int) ..] rows))


renderValuesRow :: Ast.ExprList -> Doc ann
renderValuesRow exprs =
    let items = toL exprs
        rendered = map renderExpr items
     in case items of
            [_] -> "(" <> commaJoin rendered <> ")"
            _ -> renderBracketList rendered


renderOnConflict :: Ast.OnConflict -> Doc ann
renderOnConflict (Ast.OnConflict mConfExpr onDo) =
    let header = "on conflict" <> maybe mempty (\ce -> " " <> inline render ce) mConfExpr
     in header <> hardline <> renderOnConflictDo onDo


renderOnConflictDo :: Ast.OnConflictDo -> Doc ann
renderOnConflictDo = \case
    Ast.NothingOnConflictDo -> "do nothing"
    Ast.UpdateOnConflictDo setClauses mWhere ->
        let items = toL setClauses
            base = case items of
                [single] ->
                    "do update set"
                        <> hardline
                        <> indent indentSize (renderSetClause single)
                _ ->
                    let renderItem i clause =
                            let d = renderSetClause clause
                             in if i < length items - 1
                                    then d <> ","
                                    else d
                     in "do update set"
                            <> hardline
                            <> indent indentSize (vsep (zipWith renderItem [(0 :: Int) ..] items))
         in case mWhere of
                Nothing -> base
                Just w -> base <> hardline <> renderWhereClause (Ast.WhereClause w)


-- | Render UPDATE statement.
renderUpdateStmt :: Ast.UpdateStmt -> Doc ann
renderUpdateStmt (Ast.UpdateStmt mWith rel setClauses mFrom mWhere mReturning) =
    vsep $
        catMaybes
            [ renderWithClause <$> mWith
            , Just ("update" <+> inline render rel)
            , Just (renderSetClauseLines setClauses)
            , renderFromClause <$> mFrom
            , renderWhereOrCurrent <$> mWhere
            , (\(Ast.ReturningClause rc) -> "returning" <+> renderTargetListInline rc) <$> mReturning
            ]


-- | Render DELETE statement.
renderDeleteStmt :: Ast.DeleteStmt -> Doc ann
renderDeleteStmt (Ast.DeleteStmt mWith rel mUsing mWhere mReturning) =
    vsep $
        catMaybes
            [ renderWithClause <$> mWith
            , Just ("delete from" <+> inline render rel)
            , (\(Ast.UsingClause u) -> "using" <+> inline render u) <$> mUsing
            , renderWhereOrCurrent <$> mWhere
            , (\(Ast.ReturningClause rc) -> "returning" <+> renderTargetListInline rc) <$> mReturning
            ]


renderWhereOrCurrent :: Ast.WhereOrCurrentClause -> Doc ann
renderWhereOrCurrent = \case
    Ast.ExprWhereOrCurrentClause expr -> renderWhereClause (Ast.WhereClause expr)
    Ast.CursorWhereOrCurrentClause name -> "where current of" <+> inline render name


-- | Multi-assignment SETs put `set` on its own line and use the same
-- block-list layout as multi-target SELECTs. Single-assignment SETs stay
-- inline (`set col = val`).
renderSetClauseLines :: Ast.SetClauseList -> Doc ann
renderSetClauseLines setClauses =
    let items = toL setClauses
     in case items of
            [single] -> "set" <+> renderSetClause single
            _ -> renderBlockListWith "set" renderSetClause items
