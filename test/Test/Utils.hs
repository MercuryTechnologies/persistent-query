{-# LANGUAGE OverloadedStrings #-}

module Test.Utils where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Morph (hoist)
import Data.Set qualified as Set
import Data.Void (absurd)
import Database.Esqueleto.Experimental qualified as E (insertEntityMany)
import Database.Persist.Query.Common qualified as Q
import Database.Persist.Sqlite qualified as P
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
  _ <- P.runMigrationQuiet testMigrations
  -- Need the table without primary key to simluate patholgoical behavior of duplicate keys
  P.rawExecute "DROP TABLE test_table;" []
  P.rawExecute "CREATE TABLE test_table (id INTEGER NOT NULL, value INTEGER);" []
  initialization
  Q.unQuery (Q.withException q) (const absurd) (hoist liftIO <$> Q.runQueryPart)

emptyList :: [Int]
emptyList = []

genListElement :: Gen Int
genListElement = Gen.int (Range.linear 0 100)

-- Two or more elements
genList :: Int -> Int -> Gen [Int]
genList minElements maxElements = Gen.list (Range.linear minElements maxElements) genListElement

genUniqueList :: Int -> Int -> Gen [Int]
genUniqueList minElements maxElements =
  Set.toList <$> Gen.set (Range.linear minElements maxElements) (Gen.int Range.constantBounded)

populateTestTable :: [Int] -> MonadIO m => P.SqlPersistT m ()
populateTestTable ids = E.insertEntityMany (map createEntity ids)
  where
    createEntity id = P.Entity (TestTableKey id) TestTable

