{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}


module Database.Persist.Query.CommonTest (tests) where

import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Database.Esqueleto.Experimental qualified as E
import Database.Persist.Query.WithError qualified as Q
import GHC.Stack
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Models
import Test.Utils


baseSelect :: Int -> E.SqlQuery (E.SqlExpr (E.Value TestTableId))
baseSelect x = do
  t <- E.from $ E.table @TestTable
  E.where_ $ (t E.^. TestTableId) E.==. E.val (TestTableKey x)
  pure (t E.^. TestTableId)


--
-- query
--

prop_query_noRows :: Property
prop_query_noRows = property $ do
  -- searches for id 0, which we don't pouplate
  xs <- forAll $ genUniqueList 0 10
  Right actual <- testQueryWithInit
    (populateTestTable xs)
    (Q.query $ baseSelect 0)
  diff actual (==) []

prop_query_oneOrMoreRows :: Property
prop_query_oneOrMoreRows = property $ do
  xs@(x : _) <- forAll $ genUniqueList 1 10
  Right actual <- testQueryWithInit
    (populateTestTable xs)
    ((fmap . fmap) (unTestTableKey . E.unValue) . Q.query $ baseSelect x)
  diff actual (==) [x]

--
-- notExists
--

prop_notExists_noRows :: Property
prop_notExists_noRows = property $ do
  x <- forAll genListElement
  Right actual <- testQuery (Q.notExists $ baseSelect x)
  diff actual (==) True

prop_notExists :: Property
prop_notExists = property $ do
  xs@(x : _) <- forAll $ genUniqueList 1 10
  Right actual1 <- testQueryWithInit
    (populateTestTable xs)
    -- searches for id 0, which we don't pouplate
    (Q.notExists $ baseSelect 0)
  diff actual1 (==) True

  Right actual2 <- testQueryWithInit
    (populateTestTable xs)
    (Q.notExists $ baseSelect x)
  diff actual2 (==) False


--
-- exists
--

prop_exists_noRows :: Property
prop_exists_noRows = property $ do
  x <- forAll genListElement
  Right actual <- testQuery (Q.exists $ baseSelect x)
  diff actual (==) False

prop_exists :: Property
prop_exists = property $ do
  xs@(x : _) <- forAll $ genUniqueList 1 10
  Right actual1 <- testQueryWithInit
    (populateTestTable xs)
    (Q.exists $ baseSelect x)
  diff actual1 (==) True

  Right actual2 <- testQueryWithInit
    (populateTestTable xs)
    -- searching for 0, a non existing key
    (Q.exists $ baseSelect 0)
  diff actual2 (==) False


--
-- first
--

prop_first_noRows :: Property
prop_first_noRows = withTests 1 $ property $ do
  diff (Q.first emptyList) (==) Nothing

prop_first_oneOrManyRow :: Property
prop_first_oneOrManyRow = property $ do
   xs@(x : _) <- forAll $ genList 2 10
   diff (Q.first xs) (==) (Just x)

--
-- withDefault
--

prop_withDefault_nothing :: Property
prop_withDefault_nothing = withTests 1 $ property $ do
  let def = -1
  actual <- testQuery $ Q.withDefault @TestError def Nothing
  diff actual (==) (Right def)

prop_withDefault_just :: Property
prop_withDefault_just = property $ do
  let def = -1
  x <- forAll genListElement
  actual <- testQuery $ Q.withDefault @TestError def (Just x)
  diff actual (==) (Right x)

--
-- maybeOne
--

prop_maybeOne_noRows :: Property
prop_maybeOne_noRows = withTests 1 $ property $ do
  actual <- testQuery $ Q.maybeOne @TestError emptyList
  diff actual (==) (Right Nothing)

prop_maybeOne_oneOrMoreRows :: Property
prop_maybeOne_oneOrMoreRows = property $ do
  xs@(x : _)  <- forAll $ genList 1 10
  actual <- testQuery $ Q.maybeOne @TestError xs
  diff actual (==) (Right $ Just x)

--
-- getFirstRecordByField
--

prop_getFirstRecordByField :: Property
prop_getFirstRecordByField = property $ do
  xs@(x : _) <- forAll $ genUniqueList 1 10
  Right actual1 <- testQueryWithInit
    (populateTestTable xs)
    -- searches for val 0, which is set for every record
    (Q.getFirstRecordByField TestTableVal2 0)
  assert $ isJust actual1

  Right actual2 <- testQueryWithInit
    (populateTestTable xs)
    -- searches for val 1, which is in no record
    (Q.getFirstRecordByField TestTableVal2 1)
  assert $ isNothing actual2


tests :: IO Bool
tests = do
  checkSequential $$discover
