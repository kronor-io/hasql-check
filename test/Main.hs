{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Exception (SomeException, bracket_, try)
import Control.Monad (void)
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Either (isLeft)
import Data.Int (Int32, Int64)
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Vector qualified as Vector
import Hasql.Check.Inferred (render)
import Hasql.Check.Inferred.TH qualified as Inferred
import Hasql.Check.Lexer (AlexPosn (..))
import Hasql.Check.Parser (QuasiQuote (..), parseQuasiQuotes)
import Hasql.Check.TH qualified as Checked
import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Session qualified as Session
import Hasql.Statement (Statement)
import Language.Haskell.TH (runQ)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Pqi.Ffi qualified
import System.Directory (createDirectoryIfMissing, findExecutable)
import System.Environment (getEnvironment, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess, proc, readCreateProcessWithExitCode, readProcess)
import System.Process qualified as Process
import Test.Syd
import Prelude


main :: IO ()
main = sydTest do
    describe "parseQuasiQuotes" lexerSpec
    describe "hasql-check" $ aroundAll withPostgres executableSpec
    describe "hasql-check --nullability" $ aroundAll withPostgres nullabilitySpec
    describe "hasql-check --types" $ aroundAll withPostgres typesSpec
    describe "suppressing warnings" $ aroundAll withPostgres suppressionSpec
    describe "Hasql.Check.Inferred.TH" piecesSpec
    describe "inferred statements" $ aroundAll withPostgres runtimeSpec
    describe "hasql-check --inferred" $ aroundAll withPostgres inferredSpec
    -- These share the process's environment and the module's one connection,
    -- so they run one at a time, in order.
    describe "Hasql.Check.TH" $ sequential $ doNotRandomiseExecutionOrder $ aroundAll withPostgres thSpec


-- | The quoter, its line and column, and the SQL, of every quasiquote found.
quotes :: Text -> Either String [(Text, Int, Int, Text)]
quotes source =
    map (\(QuasiQuote (AlexPn _ line col, quoter) (_, sql)) -> (quoter, line, col, sql))
        <$> parseQuasiQuotes (ByteString.Lazy.fromStrict (Text.Encoding.encodeUtf8 source))


lexerSpec :: Spec
lexerSpec = do
    it "finds a quote" $
        quotes "f = [singletonStatement|select 1 :: int4|]\n"
            `shouldBe` Right [("singletonStatement", 1, 5, "select 1 :: int4")]

    it "finds a qualified quote" $
        quotes "f = [Hasql.TH.vectorStatement|select 1 :: int4|]\n"
            `shouldBe` Right [("Hasql.TH.vectorStatement", 1, 5, "select 1 :: int4")]

    it "keeps the newlines inside a quote" $
        quotes "f =\n  [resultlessStatement|\n    delete from t\n  |]\n"
            `shouldBe` Right [("resultlessStatement", 2, 3, "\n    delete from t\n  ")]

    it "skips a quote in a line comment" $
        quotes "-- f = [singletonStatement|select 1 :: int4|]\n"
            `shouldBe` Right []

    it "skips a quote in a nested block comment" $
        quotes "{- {- -} [singletonStatement|select 1 :: int4|] -}\n"
            `shouldBe` Right []

    it "does not read an operator made of dashes as a comment" $
        quotes "a --> b = [singletonStatement|select 1 :: int4|]\n"
            `shouldBe` Right [("singletonStatement", 1, 11, "select 1 :: int4")]

    it "does not open a comment from \"{-\" in a string" $
        quotes "s = \"{-\"\nf = [singletonStatement|select 1 :: int4|]\n"
            `shouldBe` Right [("singletonStatement", 2, 5, "select 1 :: int4")]

    it "does not open a quote from \"[x|\" in a string" $
        quotes "s = \"[x|\"\nf = [singletonStatement|select 1 :: int4|]\n"
            `shouldBe` Right [("singletonStatement", 2, 5, "select 1 :: int4")]

    it "does not open a string from a double quote character literal" $
        quotes "c = '\"'\nf = [singletonStatement|select 1 :: int4|]\n"
            `shouldBe` Right [("singletonStatement", 2, 5, "select 1 :: int4")]

    it "does not open a character literal from a primed identifier" $
        quotes "g x' y' = \"{-\"\nf = [singletonStatement|select 1 :: int4|]\n"
            `shouldBe` Right [("singletonStatement", 2, 5, "select 1 :: int4")]

    it "skips escaped quotes and gaps in a string" $
        quotes "s = \"a\\\"{-\\\n    \\b\"\nf = [singletonStatement|select 1 :: int4|]\n"
            `shouldBe` Right [("singletonStatement", 3, 5, "select 1 :: int4")]

    it "skips a multiline string" $
        quotes "s = \"\"\"\n  say \"hi\" {-\n  \"\"\"\nf = [singletonStatement|select 1 :: int4|]\n"
            `shouldBe` Right [("singletonStatement", 4, 5, "select 1 :: int4")]

    it "rejects a quote left open" $
        quotes "f = [singletonStatement|select 1 :: int4\n"
            `shouldSatisfy` either ("not closed" `isInfixOf`) (const False)


-- | A connection string for a throwaway server, reachable only over a socket
-- in a temporary directory.
withPostgres :: (Text -> IO ()) -> IO ()
withPostgres action =
    withSystemTempDirectory "hasql-check" \dir -> do
        initdb <- findExecutable "initdb"
        case initdb of
            Nothing -> expectationFailure "initdb is not on PATH; the executable tests need a PostgreSQL installation"
            Just _ -> pure ()
        let dataDir = dir </> "data"
            conn = Text.pack ("host=" <> dir <> " port=5432 user=postgres dbname=postgres")
        _ <- readProcess "initdb" ["--pgdata", dataDir, "--username", "postgres", "--auth", "trust", "--no-sync"] ""
        bracket_
            (callProcess "pg_ctl" ["--pgdata", dataDir, "--log", dir </> "postgres.log", "--options", "-k " <> dir <> " -c listen_addresses=''", "--wait", "--silent", "start"])
            (callProcess "pg_ctl" ["--pgdata", dataDir, "--mode", "immediate", "--silent", "stop"])
            do
                callProcess "psql" [Text.unpack conn, "--quiet", "--command", "create table account (id int4 primary key, name text not null, nickname text); create index on account (nickname); create table note (account_id int4 not null references account, body text not null); create domain cents as int8; create table ledger (id int8 primary key, small int2, amount int8 not null, rate float8, at timestamptz, price cents); create table tag (id int4 primary key, label varchar(20) not null); insert into tag values (1, 'red')"]
                action conn


-- | Run the executable on a source file and the given flags, with extra
-- environment variables; return the exit code and stdout.
runOn :: [(String, String)] -> [String] -> Text -> IO (ExitCode, String)
runOn env args source =
    withSystemTempDirectory "hasql-check-src" \dir -> do
        let file = dir </> "Fixture.hs"
        ByteString.Lazy.writeFile file (ByteString.Lazy.fromStrict (Text.Encoding.encodeUtf8 source))
        inherited <- getEnvironment
        let unset = ["DATABASE_URL", "psql_conn_string"]
            procEnv = env <> filter ((`notElem` (unset <> map fst env)) . fst) inherited
        (code, out, err) <-
            readCreateProcessWithExitCode
                (proc "hasql-check" (args <> [file])){Process.env = Just procEnv}
                ""
        pure (code, out <> err)


executableSpec :: TestDef '[Text] ()
executableSpec = do
    itWithOuter "accepts a statement that matches the schema" \conn -> do
        (code, out) <- runOn [] ["--database", Text.unpack conn] "f = [singletonStatement|select name :: text from account where id = $1 :: int4|]\n"
        (code, out) `shouldBe` (ExitSuccess, "")

    itWithOuter "rejects a statement that does not, and says where it is" \conn -> do
        (code, out) <- runOn [] ["--database", Text.unpack conn] "f = 1\ng = [singletonStatement|select nope :: text from account|]\n"
        code `shouldBe` ExitFailure 1
        out `shouldSatisfy` ("Fixture.hs:2:5" `isInfixOf`)
        out `shouldSatisfy` ("column \"nope\" does not exist" `isInfixOf`)

    itWithOuter "accepts nullability markers" \conn -> do
        (code, out) <- runOn [] ["--database", Text.unpack conn] "f = [maybeStatement|select name :: text? from account where id = $1 :: int4?|]\n"
        (code, out) `shouldBe` (ExitSuccess, "")

    syntaxSpec

    itWithOuter "checks a qualified quote" \conn -> do
        (code, out) <- runOn [] ["--database", Text.unpack conn] "f = [Hasql.TH.resultlessStatement|delete from nowhere|]\n"
        code `shouldBe` ExitFailure 1
        out `shouldSatisfy` ("relation \"nowhere\" does not exist" `isInfixOf`)

    itWithOuter "leaves uncheckedSql alone" \conn -> do
        (code, out) <- runOn [] ["--database", Text.unpack conn] "f = [uncheckedSql|create table t (); create table u ();|]\n"
        (code, out) `shouldBe` (ExitSuccess, "")

    itWithOuter "checks only the extra quoters it is given" \conn -> do
        let source = "f = [myStatement|select nope :: text from account|]\n"
        (codeWithout, _) <- runOn [] ["--database", Text.unpack conn] source
        codeWithout `shouldBe` ExitSuccess
        (codeWith, out) <- runOn [] ["--database", Text.unpack conn, "--quoter", "myStatement"] source
        codeWith `shouldBe` ExitFailure 1
        out `shouldSatisfy` ("column \"nope\" does not exist" `isInfixOf`)

    itWithOuter "connects through DATABASE_URL" \conn -> do
        (code, out) <- runOn [("DATABASE_URL", Text.unpack conn)] [] "f = [singletonStatement|select nope :: text from account|]\n"
        code `shouldBe` ExitFailure 1
        out `shouldSatisfy` ("column \"nope\" does not exist" `isInfixOf`)

    it "lists the quotes it would check without connecting" do
        (code, out) <-
            runOn
                [("DATABASE_URL", "host=/nonexistent")]
                ["--list"]
                "f = [singletonStatement|select 1 :: int4|]\ng = [Hasql.TH.vectorStatement|select 1 :: int4|]\nh = [aesonQQ|{}|]\n"
        code `shouldBe` ExitSuccess
        lines out `shouldSatisfy` \case
            [first, second] ->
                "Fixture.hs:1:5: singletonStatement" `isSuffixOfStr` first
                    && "Fixture.hs:2:5: Hasql.TH.vectorStatement" `isSuffixOfStr` second
            _ -> False
  where
    isSuffixOfStr suffix s = Text.pack suffix `Text.isSuffixOf` Text.pack s


-- | The syntax only Kronor's fork of hasql-th accepts.
syntaxSpec :: TestDef '[Text] ()
#ifdef KRONOR_SYNTAX
syntaxSpec = do
    itWithOuter "accepts Haskell targets and param fields" \conn -> do
        (code, out) <- runOn [] ["--database", Text.unpack conn] "f = [maybeStatement|select $Just (name :: text) from account where id = $1.$accountId :: int4|]\n"
        (code, out) `shouldBe` (ExitSuccess, "")

    itWithOuter "checks the columns inside a Haskell target" \conn -> do
        (code, out) <- runOn [] ["--database", Text.unpack conn] "f = [maybeStatement|select $Just (nope :: text) from account|]\n"
        code `shouldBe` ExitFailure 1
        out `shouldSatisfy` ("column \"nope\" does not exist" `isInfixOf`)
#else
syntaxSpec =
    itWithOuter "rejects Haskell targets, which upstream hasql-th does not accept" \conn -> do
        (code, out) <- runOn [] ["--database", Text.unpack conn] "f = [maybeStatement|select $Just (name :: text) from account|]\n"
        code `shouldBe` ExitFailure 1
        out `shouldSatisfy` ("Fixture.hs:1:5" `isInfixOf`)
#endif


-- | Run with --nullability on one statement; return the exit code and output.
nullability :: Text -> Text -> IO (ExitCode, String)
nullability conn statement =
    runOn [] ["--database", Text.unpack conn, "--nullability"] ("f = [vectorStatement|" <> statement <> "|]\n")


nullabilitySpec :: TestDef '[Text] ()
nullabilitySpec = do
    itWithOuter "warns about a nullable column annotated non-null, without failing" \conn -> do
        (code, out) <- nullability conn "select nickname :: text from account"
        code `shouldBe` ExitSuccess
        out `shouldSatisfy` ("Fixture.hs:1:5" `isInfixOf`)
        out `shouldSatisfy` ("column 1 (nickname) is annotated non-null, but public.account.nickname is nullable" `isInfixOf`)

    itWithOuter "warns only when asked" \conn -> do
        (code, out) <- runOn [] ["--database", Text.unpack conn] "f = [vectorStatement|select nickname :: text from account|]\n"
        (code, out) `shouldBe` (ExitSuccess, "")

    itWithOuter "accepts a nullable column annotated nullable" \conn ->
        nullability conn "select nickname :: text? from account" `shouldReturn` (ExitSuccess, "")

    itWithOuter "accepts a NOT NULL column annotated non-null" \conn ->
        nullability conn "select name :: text, id :: int4 from account" `shouldReturn` (ExitSuccess, "")

    itWithOuter "accepts a column guarded by IS NOT NULL" \conn ->
        nullability conn "select nickname :: text from account where id > 0 and nickname is not null" `shouldReturn` (ExitSuccess, "")

    itWithOuter "accepts a guard that leaves out the table" \conn ->
        nullability conn "select a.nickname :: text from account a where nickname is not null" `shouldReturn` (ExitSuccess, "")

    itWithOuter "does not check expressions" \conn ->
        nullability conn "select count(*) :: int8, max(nickname) :: text from account" `shouldReturn` (ExitSuccess, "")

    itWithOuter "warns about a NOT NULL column on the nullable side of a left join" \conn -> do
        (code, out) <- nullability conn "select a.name :: text, n.body :: text from account a left join note n on n.account_id = a.id"
        code `shouldBe` ExitSuccess
        out `shouldSatisfy` ("column 2 (body) is annotated non-null, but comes from the nullable side of an outer join" `isInfixOf`)
        out `shouldNotSatisfy` ("column 1" `isInfixOf`)

    -- Postgres plans the joins above as right joins with the tables swapped;
    -- a lateral join cannot be swapped, so this one is planned as a left join.
    itWithOuter "warns about the nullable side of a join planned as a left join" \conn -> do
        (_, out) <- nullability conn "select a.name :: text, n.body :: text from account a left join lateral (select body from note where note.account_id = a.id limit 1) n on true"
        out `shouldSatisfy` ("column 2 (body)" `isInfixOf`)
        out `shouldNotSatisfy` ("column 1" `isInfixOf`)

    itWithOuter "warns about the nullable side of a right join only" \conn -> do
        (_, out) <- nullability conn "select a.name :: text, n.body :: text from note n right join account a on n.account_id = a.id"
        out `shouldSatisfy` ("column 2 (body)" `isInfixOf`)
        out `shouldNotSatisfy` ("column 1" `isInfixOf`)

    itWithOuter "warns about both sides of a full join" \conn -> do
        (_, out) <- nullability conn "select a.name :: text, n.body :: text from account a full join note n on n.account_id = a.id"
        out `shouldSatisfy` ("column 1 (name)" `isInfixOf`)
        out `shouldSatisfy` ("column 2 (body)" `isInfixOf`)

    itWithOuter "lets IS NOT NULL clear the nullable side of a join" \conn ->
        nullability conn "select n.body :: text from account a left join note n on n.account_id = a.id where n.body is not null" `shouldReturn` (ExitSuccess, "")

    itWithOuter "does not let an UPDATE's filter on the old row clear RETURNING's new one" \conn -> do
        (_, out) <- nullability conn "update account set nickname = NULL where nickname is not null returning nickname :: text"
        out `shouldSatisfy` ("public.account.nickname is nullable" `isInfixOf`)

    itWithOuter "lets a DELETE's filter clear RETURNING, which returns the rows it read" \conn ->
        nullability conn "delete from account where nickname is not null returning nickname :: text" `shouldReturn` (ExitSuccess, "")

    -- With sequential scans off, Postgres answers the OR with a BitmapOr of
    -- two index scans, one of which has the IS NOT NULL as its condition.
    itWithOuter "does not take one branch of a BitmapOr for a guarantee" \conn -> do
        (_, out) <-
            runOn
                [("PGOPTIONS", "-c enable_seqscan=off")]
                ["--database", Text.unpack conn, "--nullability"]
                "f = [vectorStatement|select nickname :: text from account where nickname is not null or id = $1 :: int4|]\n"
        out `shouldSatisfy` ("public.account.nickname is nullable" `isInfixOf`)

    itWithOuter "checks a statement with parameters" \conn -> do
        (_, out) <- nullability conn "select nickname :: text from account where id = $1 :: int4"
        out `shouldSatisfy` ("public.account.nickname is nullable" `isInfixOf`)

    itWithOuter "checks RETURNING" \conn -> do
        (_, out) <- nullability conn "update account set name = name returning nickname :: text, name :: text"
        out `shouldSatisfy` ("column 1 (nickname)" `isInfixOf`)
        out `shouldNotSatisfy` ("column 2" `isInfixOf`)


-- | Run a checked quoter as a splice would. Outside GHC a failing splice only
-- says that it failed - the message goes to stderr - so the tests that expect
-- a failure pair it with the same statement passing when nothing is checked.
quote :: QuasiQuoter -> String -> IO (Either String ())
quote quoter sql = either (Left . show) (const (Right ())) <$> (try (void (runQ (quoteExp quoter sql))) :: IO (Either SomeException ()))


thSpec :: TestDef '[Text] ()
thSpec = do
    it "checks nothing when no database is set" do
        unsetEnv "HASQL_CHECK_DATABASE_URL"
        quote Checked.vectorStatement "select nope :: text from account" `shouldReturn` Right ()
        quote Checked.resultlessStatement "delete from nowhere" `shouldReturn` Right ()

    it "does not fail the build when the database cannot be reached" do
        setEnv "HASQL_CHECK_DATABASE_URL" "host=/nonexistent"
        quote Checked.maybeStatement "select nope :: text from account" `shouldReturn` Right ()

    itWithOuter "accepts a statement that prepares" \conn -> do
        setEnv "HASQL_CHECK_DATABASE_URL" (Text.unpack conn)
        quote Checked.maybeStatement "select name :: text from account where id = $1 :: int4" `shouldReturn` Right ()

    itWithOuter "fails on a statement that does not" \conn -> do
        setEnv "HASQL_CHECK_DATABASE_URL" (Text.unpack conn)
        result <- quote Checked.vectorStatement "select nope :: text from account"
        result `shouldSatisfy` isLeft

    itWithOuter "checks statements without rows" \conn -> do
        setEnv "HASQL_CHECK_DATABASE_URL" (Text.unpack conn)
        result <- quote Checked.resultlessStatement "delete from nowhere"
        result `shouldSatisfy` isLeft

    -- Run after the tests above have left a connection open to the test
    -- database: a broken statement fails there, but not when the database
    -- named cannot be reached.
    it "follows a change of database" do
        setEnv "HASQL_CHECK_DATABASE_URL" "host=/nonexistent"
        quote Checked.vectorStatement "select nope :: text from account" `shouldReturn` Right ()


-- | Run with --types on one quote; return the exit code and output.
types :: Text -> Text -> Text -> IO (ExitCode, String)
types conn quoter statement =
    runOn [] ["--database", Text.unpack conn, "--types"] ("f = [" <> quoter <> "|" <> statement <> "|]\n")


typesSpec :: TestDef '[Text] ()
typesSpec = do
    itWithOuter "warns about a column read as a narrower type, without failing" \conn -> do
        (code, out) <- types conn "vectorStatement" "select amount :: int4 from ledger"
        code `shouldBe` ExitSuccess
        out `shouldSatisfy` ("column 1 (amount) is annotated integer, but is a bigint: it may overflow" `isInfixOf`)

    itWithOuter "warns only when asked" \conn ->
        runOn [] ["--database", Text.unpack conn] "f = [vectorStatement|select amount :: int4 from ledger|]\n" `shouldReturn` (ExitSuccess, "")

    itWithOuter "accepts a column read as its own type, or a wider one" \conn ->
        types conn "vectorStatement" "select amount :: int8, small :: int4, id :: numeric from ledger" `shouldReturn` (ExitSuccess, "")

    itWithOuter "does not check expressions" \conn ->
        types conn "vectorStatement" "select count(*) :: int4, now() :: date from ledger" `shouldReturn` (ExitSuccess, "")

    itWithOuter "warns about lost precision" \conn -> do
        (_, out) <- types conn "vectorStatement" "select rate :: float4 from ledger"
        out `shouldSatisfy` ("is annotated real, but is a double precision: it may lose precision" `isInfixOf`)

    itWithOuter "warns about a dropped time of day" \conn -> do
        (_, out) <- types conn "vectorStatement" "select at :: date from ledger"
        out `shouldSatisfy` ("is annotated date, but is a timestamptz" `isInfixOf`)

    itWithOuter "looks through domains" \conn -> do
        (_, out) <- types conn "vectorStatement" "select price :: int4 from ledger"
        out `shouldSatisfy` ("column 1 (price) is annotated integer, but is a bigint" `isInfixOf`)

    -- Postgres reports a result column of a domain type as the base type, but
    -- a parameter assigned to one as the domain.
    itWithOuter "looks through a domain a parameter is assigned to" \conn -> do
        (_, out) <- types conn "resultlessStatement" "insert into ledger (id, amount, price) values ($1 :: int8, $2 :: int8, $3 :: numeric)"
        out `shouldSatisfy` ("parameter $3 is annotated numeric, but is assigned to a bigint column: it may be rounded, or overflow" `isInfixOf`)

    itWithOuter "warns about a parameter inserted into a narrower column" \conn -> do
        (_, out) <- types conn "resultlessStatement" "insert into account (id, name) values ($1 :: int8, $2 :: text)"
        out `shouldSatisfy` ("parameter $1 is annotated bigint, but is assigned to an integer column: it may overflow" `isInfixOf`)
        out `shouldNotSatisfy` ("parameter $2" `isInfixOf`)

    itWithOuter "accepts a parameter inserted into a column of its type" \conn ->
        types conn "resultlessStatement" "insert into account (id, name) values ($1 :: int4, $2 :: text)" `shouldReturn` (ExitSuccess, "")

    itWithOuter "checks parameters set by UPDATE, not those compared" \conn -> do
        (_, out) <- types conn "resultlessStatement" "update ledger set small = $1 :: int4 where amount = $2 :: int4"
        out `shouldSatisfy` ("parameter $1 is annotated integer, but is assigned to a smallint column" `isInfixOf`)
        out `shouldNotSatisfy` ("parameter $2" `isInfixOf`)

    itWithOuter "checks every column a parameter is assigned to" \conn -> do
        (_, out) <- types conn "resultlessStatement" "update ledger set amount = $1 :: int8, small = $1 :: int8 where id = $2 :: int8"
        out `shouldSatisfy` ("parameter $1 is annotated bigint, but is assigned to a smallint column" `isInfixOf`)

    itWithOuter "removes an annotation from ORDER BY too, so DISTINCT still prepares" \conn -> do
        (_, out) <- types conn "vectorStatement" "select distinct amount :: int4 from ledger order by amount :: int4"
        out `shouldSatisfy` ("column 1 (amount) is annotated integer, but is a bigint" `isInfixOf`)

    itWithOuter "removes an annotation from GROUP BY too" \conn -> do
        (_, out) <- types conn "vectorStatement" "select amount :: int4 from ledger group by amount :: int4"
        out `shouldSatisfy` ("column 1 (amount) is annotated integer, but is a bigint" `isInfixOf`)

    itWithOuter "checks RETURNING along with the assignments" \conn -> do
        (_, out) <- types conn "singletonStatement" "insert into account (id, name) values ($1 :: int8, $2 :: text) returning id :: int2"
        out `shouldSatisfy` ("column 1 (id) is annotated smallint, but is an integer" `isInfixOf`)
        out `shouldSatisfy` ("parameter $1" `isInfixOf`)


-- | Run on a whole source with the given flags; return the exit code and output.
checkSource :: Text -> [String] -> Text -> IO (ExitCode, String)
checkSource conn flags = runOn [] (["--database", Text.unpack conn] <> flags)


suppressionSpec :: TestDef '[Text] ()
suppressionSpec = do
    itWithOuter "a directive above the quote suppresses the finding it names" \conn ->
        checkSource conn ["--nullability"] "-- hasql-check: allow nullable(nickname)\nf = [vectorStatement|select nickname :: text from account|]\n"
            `shouldReturn` (ExitSuccess, "")

    itWithOuter "a directive suppresses only what it names, and says what it did not use" \conn -> do
        (_, out) <- checkSource conn ["--nullability"] "-- hasql-check: allow nullable(name)\nf = [vectorStatement|select nickname :: text from account|]\n"
        out `shouldSatisfy` ("column 1 (nickname) is annotated non-null" `isInfixOf`)
        out `shouldSatisfy` ("allow nullable(name) suppresses nothing" `isInfixOf`)

    itWithOuter "a directive applies only directly above the quote" \conn -> do
        (_, out) <- checkSource conn ["--nullability"] "-- hasql-check: allow nullable(nickname)\n\nf = [vectorStatement|select nickname :: text from account|]\n"
        out `shouldSatisfy` ("column 1 (nickname) is annotated non-null" `isInfixOf`)

    itWithOuter "directives read every comment line directly above" \conn ->
        checkSource
            conn
            ["--nullability", "--types"]
            "f =\n    -- hasql-check: allow narrowing(amount)\n    -- Why: the amounts here are small.\n    -- hasql-check: allow nullable(nickname)\n    [vectorStatement|select nickname :: text, amount :: int4 from account, ledger|]\n"
            `shouldReturn` (ExitSuccess, "")

    itWithOuter "a directive names parameters as $n" \conn ->
        checkSource conn ["--types"] "-- hasql-check: allow narrowing($1)\nf = [resultlessStatement|insert into account (id, name) values ($1 :: int8, $2 :: text)|]\n"
            `shouldReturn` (ExitSuccess, "")

    itWithOuter "a directive for a rule that was not checked is not reported" \conn ->
        checkSource conn ["--nullability"] "-- hasql-check: allow narrowing(amount)\nf = [vectorStatement|select name :: text from account|]\n"
            `shouldReturn` (ExitSuccess, "")

    itWithOuter "a malformed directive is reported" \conn -> do
        (_, out) <- checkSource conn ["--nullability"] "-- hasql-check: permit nullable(nickname)\nf = [vectorStatement|select name :: text from account|]\n"
        out `shouldSatisfy` ("directive not understood: permit nullable(nickname)" `isInfixOf`)

    itWithOuter "--fail-on-warnings fails on a finding, not on a suppressed one" \conn -> do
        (code, _) <- checkSource conn ["--nullability", "--fail-on-warnings"] "f = [vectorStatement|select nickname :: text from account|]\n"
        code `shouldBe` ExitFailure 1
        (suppressed, _) <- checkSource conn ["--nullability", "--fail-on-warnings"] "-- hasql-check: allow nullable(nickname)\nf = [vectorStatement|select nickname :: text from account|]\n"
        suppressed `shouldBe` ExitSuccess

    itWithOuter "a baseline ignores the findings it was written with, wherever the quote is" \conn ->
        withSystemTempDirectory "hasql-check-baseline" \dir -> do
            let baseline = dir </> "baseline"
                flags = ["--nullability", "--fail-on-warnings"]
            (written, _) <- checkSource conn (flags <> ["--write-baseline", baseline]) "f = [vectorStatement|select nickname :: text from account|]\n"
            written `shouldBe` ExitFailure 1
            contents <- readFile baseline
            contents `shouldSatisfy` (" nullable nickname # " `isInfixOf`)
            -- The same statement, laid out differently and somewhere else.
            checkSource conn (flags <> ["--baseline", baseline]) "g = 1\nf =\n  [vectorStatement|\n    select   nickname :: text\n    from account\n  |]\n"
                `shouldReturn` (ExitSuccess, "")
            (changed, out) <- checkSource conn (flags <> ["--baseline", baseline]) "f = [vectorStatement|select nickname :: text from account where id > 0|]\n"
            changed `shouldBe` ExitFailure 1
            out `shouldSatisfy` ("column 1 (nickname)" `isInfixOf`)

    itWithOuter "--files0-from checks every listed file in one run, so the baseline has them all" \conn ->
        withSystemTempDirectory "hasql-check-files" \dir -> do
            let a = dir </> "A.hs"
                b = dir </> "B's.hs"
                list = dir </> "files"
                baseline = dir </> "baseline"
            writeFile a "f = [vectorStatement|select nickname :: text from account|]\n"
            writeFile b "g = [vectorStatement|select amount :: int4 from ledger|]\n"
            writeFile list (a <> "\0" <> b <> "\0")
            (code, _, _) <-
                readCreateProcessWithExitCode
                    (proc "hasql-check" ["--database", Text.unpack conn, "--nullability", "--types", "--files0-from", list, "--write-baseline", baseline])
                    ""
            code `shouldBe` ExitSuccess
            contents <- readFile baseline
            contents `shouldSatisfy` (" nullable nickname # " `isInfixOf`)
            contents `shouldSatisfy` (" narrowing amount # " `isInfixOf`)

    itWithOuter "a baseline and a directive handle a quoted column name" \conn ->
        withSystemTempDirectory "hasql-check-quoted" \dir -> do
            let baseline = dir </> "baseline"
                source = "f = [vectorStatement|select nickname :: text as \"display name\" from account|]\n"
            _ <- checkSource conn ["--nullability", "--write-baseline", baseline] source
            contents <- readFile baseline
            contents `shouldSatisfy` (" nullable \"display name\" # " `isInfixOf`)
            checkSource conn ["--nullability", "--fail-on-warnings", "--baseline", baseline] source `shouldReturn` (ExitSuccess, "")
            checkSource conn ["--nullability", "--fail-on-warnings"] ("-- hasql-check: allow nullable(\"display name\")\n" <> source)
                `shouldReturn` (ExitSuccess, "")


-- | The SQL the inferred quoters send for a statement, given the Postgres
-- types of its parameters and result columns.
sent :: Text -> [Text] -> [Text] -> Either String Text
sent sql paramTypes columnTypes = render paramTypes columnTypes <$> Inferred.template sql


piecesSpec :: Spec
piecesSpec = do
    it "annotates each parameter, and casts each result column around the statement" $
        sent "select email, nickname from customer where id = $1" ["int8"] ["text", "text"]
            `shouldBe` Right "WITH hasql_check_result (c1, c2) AS (SELECT email, nickname FROM customer WHERE id = $1 :: int8) SELECT c1 :: text, c2 :: text FROM hasql_check_result"

    it "annotates every occurrence of a parameter" $
        sent "select a from t where b = $1 or c = $1" ["int4"] ["text"]
            `shouldBe` Right "WITH hasql_check_result (c1) AS (SELECT a FROM t WHERE b = $1 :: int4 OR c = $1 :: int4) SELECT c1 :: text FROM hasql_check_result"

    it "casts RETURNING" $
        sent "update t set a = $1 returning id" ["text"] ["int8"]
            `shouldBe` Right "WITH hasql_check_result (c1) AS (UPDATE t SET a = $1 :: text RETURNING id) SELECT c1 :: int8 FROM hasql_check_result"

    it "casts the columns behind a *" $
        sent "select * from t where id = $1" ["int8"] ["int8", "text"]
            `shouldBe` Right "WITH hasql_check_result (c1, c2) AS (SELECT * FROM t WHERE id = $1 :: int8) SELECT c1 :: int8, c2 :: text FROM hasql_check_result"

    it "leaves the statement itself as it is: its DISTINCT ON, ORDER BY and GROUP BY" $
        sent "select distinct on (t.id) t.id from t group by 1 order by id" [] ["int8"]
            `shouldBe` Right "WITH hasql_check_result (c1) AS (SELECT DISTINCT ON (t.id) t.id FROM t GROUP BY 1 ORDER BY id) SELECT c1 :: int8 FROM hasql_check_result"

    it "keeps the statement's own WITH list first, where a data-modifying one must be" $
        sent "with x as (update t set a = $1 returning a) select a from x" ["int8"] ["int8"]
            `shouldBe` Right "WITH x AS (UPDATE t SET a = $1 :: int8 RETURNING a), hasql_check_result (c1) AS (SELECT a FROM x) SELECT c1 :: int8 FROM hasql_check_result"

    it "picks a name the statement does not use" $
        sent "select a from hasql_check_result" [] ["int8"]
            `shouldBe` Right "WITH hasql_check_result_ (c1) AS (SELECT a FROM hasql_check_result) SELECT c1 :: int8 FROM hasql_check_result_"

    it "keeps text in a literal that looks like an annotation" $
        sent "select ' :: hasql_check_slot_1'" [] ["text"]
            `shouldBe` Right "WITH hasql_check_result (c1) AS (SELECT ' :: hasql_check_slot_1') SELECT c1 :: text FROM hasql_check_result"

    it "sends a statement without rows as it is" $
        sent "delete from t where id = $1" ["int8"] []
            `shouldBe` Right "DELETE FROM t WHERE id = $1 :: int8"

    it "rejects SQL it cannot parse" $
        Inferred.template "select from where" `shouldSatisfy` isLeft


-- | Run a statement on the test database.
runStatement :: Text -> params -> Statement params result -> IO result
runStatement conn params statement = do
    Right c <- Connection.acquire Pqi.Ffi.adapter (Settings.connectionString conn)
    result <- Connection.use c (Session.statement params statement)
    Connection.release c
    either (\e -> expectationFailure (show e) >> error "unreachable") pure result


runtimeSpec :: TestDef '[Text] ()
runtimeSpec = do
    itWithOuter "reads a varchar column as Text" \conn ->
        runStatement conn (1 :: Int32) [Inferred.singletonStatement|select label from tag where id = $1|]
            `shouldReturn` ("red" :: Text)

    itWithOuter "reads an int4 column as Int64, and sends an Int64 parameter for it" \conn ->
        runStatement conn (1 :: Int64) [Inferred.singletonStatement|select id from tag where id = $1|]
            `shouldReturn` (1 :: Int64)

    itWithOuter "reads a tuple, with a Maybe for a NULL" \conn ->
        runStatement conn (1 :: Int32) [Inferred.maybeStatement|select label, null from tag where id = $1|]
            `shouldReturn` Just ("red" :: Text, Nothing :: Maybe Text)

    itWithOuter "sends a literal that looks like an annotation unchanged" \conn ->
        runStatement conn () [Inferred.singletonStatement|select ' :: hasql_check_slot_c1'|]
            `shouldReturn` (" :: hasql_check_slot_c1" :: Text)

    -- Ordered by the column the alias "id" names, label, rather than v.id.
    itWithOuter "keeps the order of SELECT DISTINCT by an alias" \conn ->
        runStatement conn () [Inferred.singletonStatement|select distinct id as key, label as id from (values (2, 'a'), (1, 'b')) v (id, label) order by id limit 1|]
            `shouldReturn` (2 :: Int64, "a" :: Text)

    itWithOuter "runs SELECT DISTINCT ON, ordered by an output name, with a cast column" \conn ->
        runStatement conn () [Inferred.vectorStatement|select distinct on (t.id) t.id from tag t where t.id = 1 order by id|]
            `shouldReturn` Vector.fromList [1 :: Int64]

    itWithOuter "runs GROUP BY a primary key, with a cast column" \conn ->
        runStatement conn () [Inferred.vectorStatement|select id, label from tag where id = 1 group by 1|]
            `shouldReturn` Vector.fromList [(1 :: Int64, "red" :: Text)]

    itWithOuter "runs a statement that does not use all its parameters" \conn ->
        runStatement conn (1 :: Int64, 2 :: Int64) [Inferred.singletonStatement|select $2|]
            `shouldReturn` (2 :: Int64)

    itWithOuter "runs a statement without rows" \conn ->
        runStatement conn ("blue" :: Text, 2 :: Int64) [Inferred.resultlessStatement|insert into tag (label, id) values ($1, $2)|]
            `shouldReturn` ()


-- | A quote as hasql-check-hie writes it: its SQL, and the qualified names of
-- its parameter and result types, a nullable one prefixed with "?".
inferredQuote :: Text -> Text -> [Text] -> Maybe [Text] -> Text
inferredQuote quoter sql params row =
    "{\"file\":\"src/Fixture.hs\",\"line\":3,\"column\":7,\"quoter\":\""
        <> quoter
        <> "\",\"sql\":\""
        <> sql
        <> "\",\"params\":"
        <> columns params
        <> ",\"row\":"
        <> maybe "null" columns row
        <> "}"
  where
    columns cs = "[" <> Text.intercalate "," (map column cs) <> "]"
    column c = case Text.stripPrefix "?" c of
        Just t -> "{\"type\":\"" <> t <> "\",\"nullable\":true}"
        Nothing -> "{\"type\":\"" <> c <> "\",\"nullable\":false}"


-- | Run with --inferred on the given quotes and flags.
runInferred :: Text -> [String] -> [Text] -> IO (ExitCode, String)
runInferred conn args quotes' =
    withSystemTempDirectory "hasql-check-inferred" \dir -> do
        let file = dir </> "quotes.jsonl"
        ByteString.Lazy.writeFile file (ByteString.Lazy.fromStrict (Text.Encoding.encodeUtf8 (Text.unlines quotes')))
        (code, out, err) <- readCreateProcessWithExitCode (proc "hasql-check" (["--database", Text.unpack conn, "--inferred", file] <> args)) ""
        pure (code, out <> err)


int32, int64, text :: Text
int32 = "GHC.Internal.Int.Int32"
int64 = "GHC.Internal.Int.Int64"
text = "Data.Text.Internal.Text"


inferredSpec :: TestDef '[Text] ()
inferredSpec = do
    itWithOuter "accepts a statement whose Haskell types fit" \conn ->
        runInferred conn [] [inferredQuote "singletonStatement" "select label, id from tag where id = $1" [int64] (Just [text, int64])]
            `shouldReturn` (ExitSuccess, "")

    itWithOuter "rejects one that does not prepare, and says where it is" \conn -> do
        (code, out) <- runInferred conn [] [inferredQuote "singletonStatement" "select nope from tag" [] (Just [text])]
        code `shouldBe` ExitFailure 1
        out `shouldSatisfy` ("src/Fixture.hs:3:7" `isInfixOf`)
        out `shouldSatisfy` ("column \"nope\" does not exist" `isInfixOf`)

    itWithOuter "rejects a Haskell type with too few columns" \conn -> do
        (code, out) <- runInferred conn [] [inferredQuote "singletonStatement" "select label, id from tag" [] (Just [text])]
        code `shouldBe` ExitFailure 1
        out `shouldSatisfy` ("the statement returns 2 columns, but the Haskell type reads 1" `isInfixOf`)

    itWithOuter "accepts the columns behind a *, cast to the types read" \conn ->
        runInferred conn [] [inferredQuote "singletonStatement" "select * from tag" [] (Just [int64, text])]
            `shouldReturn` (ExitSuccess, "")

    itWithOuter "accepts a qualified * whose columns have the types read" \conn ->
        runInferred conn [] [inferredQuote "vectorStatement" "select a.* from account a" [] (Just [int32, text, "?" <> text])]
            `shouldReturn` (ExitSuccess, "")

    itWithOuter "accepts SELECT DISTINCT ordered by a column it annotates" \conn ->
        runInferred conn [] [inferredQuote "vectorStatement" "select distinct id from tag order by id" [] (Just [int64])]
            `shouldReturn` (ExitSuccess, "")

    itWithOuter "accepts SELECT DISTINCT ordered by a qualified column and an expression" \conn ->
        runInferred conn [] [inferredQuote "vectorStatement" "select distinct t.id, t.id + 1 from tag t order by t.id, t.id + 1" [] (Just [int64, int64])]
            `shouldReturn` (ExitSuccess, "")

    itWithOuter "accepts statements that a cast in place would break" \conn ->
        runInferred
            conn
            []
            [ inferredQuote "vectorStatement" "select distinct on (t.id) t.id from tag t order by id" [] (Just [int64])
            , inferredQuote "vectorStatement" "select id, label from tag group by 1" [] (Just [int64, text])
            ]
            `shouldReturn` (ExitSuccess, "")

    -- It runs, but whatever it was meant to do with $1, it does not.
    itWithOuter "rejects a statement that does not use a parameter the Haskell type sends" \conn -> do
        (code, out) <- runInferred conn [] [inferredQuote "singletonStatement" "select $2" [int64, int64] (Just [int64])]
        code `shouldBe` ExitFailure 1
        out `shouldSatisfy` ("the Haskell type sends parameter $1, which the statement does not use" `isInfixOf`)
        out `shouldNotSatisfy` ("could not determine" `isInfixOf`)

    itWithOuter "checks a quote in a source file it also has from --inferred once, as inferred" \conn ->
        withSystemTempDirectory "hasql-check-both" \dir -> do
            let source = dir </> "src" </> "Fixture.hs"
                quotesFile = dir </> "quotes.jsonl"
            createDirectoryIfMissing True (dir </> "src")
            -- Line 3, with the quote at column 7, as the inferred quote says.
            writeFile source "module Fixture where\n\ng =   [singletonStatement|select $1 + $2|]\n"
            ByteString.Lazy.writeFile quotesFile (ByteString.Lazy.fromStrict (Text.Encoding.encodeUtf8 (inferredQuote "singletonStatement" "select $1 + $2" [int64, int64] (Just [int64]))))
            (code, out, err) <- readCreateProcessWithExitCode (proc "hasql-check" ["--database", Text.unpack conn, source, "--inferred", quotesFile]) ""
            (code, out <> err) `shouldBe` (ExitSuccess, "")

    itWithOuter "rejects a Haskell type that sends more parameters than the statement uses" \conn -> do
        (code, out) <- runInferred conn [] [inferredQuote "singletonStatement" "select 1" [int64] (Just [int32])]
        code `shouldBe` ExitFailure 1
        out `shouldSatisfy` ("the Haskell type sends parameter $1, which the statement does not use" `isInfixOf`)

    itWithOuter "rejects a Haskell type that sends fewer, without rows too" \conn -> do
        (code, out) <- runInferred conn [] [inferredQuote "resultlessStatement" "delete from tag where id = $1" [] Nothing]
        code `shouldBe` ExitFailure 1
        out `shouldSatisfy` ("the statement uses $1, but the Haskell type sends 0 parameters" `isInfixOf`)

    itWithOuter "rejects a Haskell type it knows no Postgres type for" \conn -> do
        (code, out) <- runInferred conn [] [inferredQuote "singletonStatement" "select label from tag" [] (Just ["Shop.Email"])]
        code `shouldBe` ExitFailure 1
        out `shouldSatisfy` ("src/Fixture.hs:3:7: no Postgres type is known for Shop.Email" `isInfixOf`)

    itWithOuter "warns about a nullable column read without a Maybe" \conn -> do
        (_, out) <- runInferred conn ["--nullability"] [inferredQuote "vectorStatement" "select nickname from account" [] (Just [text])]
        out `shouldSatisfy` ("column 1 (nickname) is not a Maybe in Haskell, but public.account.nickname is nullable" `isInfixOf`)

    itWithOuter "accepts a nullable column read as a Maybe" \conn ->
        runInferred conn ["--nullability"] [inferredQuote "vectorStatement" "select nickname from account" [] (Just ["?" <> text])]
            `shouldReturn` (ExitSuccess, "")

    itWithOuter "warns about a column read as a narrower type" \conn -> do
        (_, out) <- runInferred conn ["--types"] [inferredQuote "vectorStatement" "select amount from ledger" [] (Just [int32])]
        out `shouldSatisfy` ("column 1 (amount) is read as integer, but is a bigint: it may overflow" `isInfixOf`)

    itWithOuter "warns about a parameter assigned to a narrower column" \conn -> do
        (_, out) <- runInferred conn ["--types"] [inferredQuote "resultlessStatement" "update ledger set small = $1" [int64] Nothing]
        out `shouldSatisfy` ("parameter $1 is sent as bigint, but is assigned to a smallint column: it may overflow" `isInfixOf`)
