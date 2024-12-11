{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}


module Database.Persist.QueryTest (tests) where

import Control.Monad.IO.Class (liftIO)
import Data.Void (absurd)
import Data.Text (unpack)
import Hedgehog
import Hedgehog.Internal.Property (failWith)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Database.Persist.Sqlite qualified as P
import Database.Persist.Query qualified as Q
import Control.Monad.Morph (hoist)



testQuery :: Q.Query e t -> PropertyT IO (Either e t)
testQuery q = liftIO $ P.runSqlite ":memory:" $ Q.unQuery (Q.withException q) (const absurd) (hoist liftIO <$> Q.runQueryPart)

isNoRows :: Q.Error -> Bool
isNoRows e = case e of
  Q.NoRows -> True
  _ -> False

isMoreThanOneRow :: Q.Error -> Bool
isMoreThanOneRow e = case e of
  Q.MoreThanOneRow -> True
  _ -> False

showError :: Q.Error -> String
showError Q.NoRows = "NoRows"
showError Q.MoreThanOneRow = "MoreThanOneRow"
showError (Q.EntityNotFound _) = "EntityNotFound"
showError (Q.MoreThanOneEntity _) = "MoreThanOneEntity"
showError (Q.Other txt) = "Other: " <> unpack txt

requireLeft :: MonadTest m => Either a b -> m a
requireLeft (Left x) = pure x
requireLeft (Right y) = failWith Nothing "Expected Left, but got Right"

requireRight :: MonadTest m => Either a b -> m b
requireRight (Left x) = failWith Nothing "Expected Right, but got Left"
requireRight (Right y) = pure y

-- Empty list returns @Left NoRows@
prop_single_noRows :: Property
prop_single_noRows = withTests 1 $ property $ do
  result <- testQuery $ Q.single ([] :: [Int])
  actual <- requireLeft result
  annotate $ showError actual
  assert $ isNoRows actual

-- Single element list returns @Right a@
prop_single_oneRow :: Property
prop_single_oneRow = property $ do
  x <- forAll $ Gen.int (Range.linear 0 100)
  result <- testQuery $ Q.single [x]
  actual <- requireRight result
  diff actual (==) x

--  List with more than one element returns @Left MoreThanOneRow@
prop_single_tooManyRows :: Property
prop_single_tooManyRows = property $ do
  xs <- forAll $ Gen.list (Range.linear 2 10) (Gen.int (Range.linear 0 100))
  result <- testQuery $ Q.single xs
  actual <- requireLeft result
  annotate $ showError actual
  assert $ isMoreThanOneRow actual



tests :: IO Bool
tests =
  checkSequential $$discover
