{-# LANGUAGE CPP #-}

module Main where

import Control.Exception (bracket_)
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Hasql.Check.Lexer (AlexPosn (..))
import Hasql.Check.Parser (QuasiQuote (..), parseQuasiQuotes)
import System.Directory (findExecutable)
import System.Environment (getEnvironment)
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
                callProcess "psql" [Text.unpack conn, "--quiet", "--command", "create table account (id int4 primary key, name text not null, nickname text); create table note (account_id int4 not null references account, body text not null)"]
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

    itWithOuter "checks a statement with parameters" \conn -> do
        (_, out) <- nullability conn "select nickname :: text from account where id = $1 :: int4"
        out `shouldSatisfy` ("public.account.nickname is nullable" `isInfixOf`)

    itWithOuter "checks RETURNING" \conn -> do
        (_, out) <- nullability conn "update account set name = name returning nickname :: text, name :: text"
        out `shouldSatisfy` ("column 1 (nickname)" `isInfixOf`)
        out `shouldNotSatisfy` ("column 2" `isInfixOf`)
