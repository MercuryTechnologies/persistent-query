{-# LANGUAGE OverloadedStrings #-}

module Test.Utils where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Morph (hoist)
import Data.Set qualified as Set
import Data.Void (absurd)
import Database.Esqueleto.Experimental qualified as E (insertEntityMany)
import Database.Persist.Query.Common qualified as Q
import Database.Persist.Sqlite qualified as P
import GHC.Stack
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Models

data TestError
  = NoRows
  | MoreThanOneRow
  deriving (Eq, Show)

testQuery :: Q.Query e t -> PropertyT IO (Either e t)
testQuery = testQueryWithInit (pure ())

testQueryWithInit :: (forall m. MonadIO m => P.SqlPersistT m ()) -> Q.Query e t -> PropertyT IO (Either e t)
testQueryWithInit initialization q = liftIO $ P.runSqlite ":memory:" $ do
  querySetup
  initialization
  Q.unQuery (Q.withException q) (const absurd) (hoist liftIO <$> Q.runQueryPart)

testQueryWithStackTrace :: Q.Query e t -> PropertyT IO (Either ([CallStack], e) t)
testQueryWithStackTrace q = liftIO $ P.runSqlite ":memory:" $ do
  querySetup
  Q.unQuery (Q.withExceptionAndStackTrace q) (const absurd) (hoist liftIO <$> Q.runQueryPart)

querySetup :: forall m. MonadIO m => P.SqlPersistT m ()
querySetup = do
  _ <- P.runMigrationQuiet testMigrations
  -- Need the table without primary key to simluate pathological behavior of duplicate keys
  P.rawExecute "DROP TABLE test_table;" []
  P.rawExecute "CREATE TABLE test_table (id INTEGER NOT NULL, val INTEGER, val2 INTEGER);" []

emptyList :: [Int]
emptyList = []

genListElement :: Gen Int
genListElement = Gen.int (Range.linear 0 100)

-- Two or more elements
genList :: Int -> Int -> Gen [Int]
genList minElements maxElements = Gen.list (Range.linear minElements maxElements) genListElement

-- | Unique list of ints from 1..(10 * maxElements).
-- Use id 0 if you want to search for something not in the list
genUniqueList :: Int -> Int -> Gen [Int]
genUniqueList minElements maxElements =
  Set.toList <$> Gen.set (Range.linear minElements maxElements) (Gen.int $ Range.linear 1 (10 * maxElements))

populateTestTable :: [Int] -> MonadIO m => P.SqlPersistT m ()
populateTestTable ids = E.insertEntityMany (map createEntity ids)
  where
    createEntity id = P.Entity (TestTableKey id) (TestTable id 0)

