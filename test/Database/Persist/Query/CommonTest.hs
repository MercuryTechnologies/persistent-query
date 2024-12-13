{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}


module Database.Persist.Query.CommonTest (tests) where

import Data.List.NonEmpty qualified as NE
import Database.Persist.Query.WithError qualified as Q
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Models
import Test.Utils

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
-- --
-- -- firstOfMany
-- --
--
-- prop_firstOfMany_noRows :: Property
-- prop_firstOfMany_noRows = withTests 1 $ property $ do
--   actual <- testQuery $ Q.firstOfMany NoRows emptyList
--   diff actual (==) (Left NoRows)
--
-- prop_firstOfMany_oneOrMoreRows :: Property
-- prop_firstOfMany_oneOrMoreRows = property $ do
--   xs@(x : _)  <- forAll $ genList 1 10
--   actual <- testQuery $ Q.firstOfMany NoRows xs
--   diff actual (==) (Right x)
--
-- --
-- -- oneOrNone
-- --
--
-- prop_oneOrNone_noRows :: Property
-- prop_oneOrNone_noRows = withTests 1 $ property $ do
--   actual <- testQuery $ Q.oneOrNone MoreThanOneRow emptyList
--   diff actual (==) (Right Nothing)
--
-- prop_oneOrNone_oneRow :: Property
-- prop_oneOrNone_oneRow = property $ do
--   x <- forAll genListElement
--   actual <- testQuery $ Q.oneOrNone MoreThanOneRow [x]
--   diff actual (==) (Right $ Just x)
--
-- --  List with more than one element returns @Left MoreThanOneRow@
-- prop_oneOrNone_tooManyRows :: Property
-- prop_oneOrNone_tooManyRows = property $ do
--   xs <- forAll $ genList 2 10
--   actual <- testQuery $ Q.oneOrNone MoreThanOneRow xs
--   diff actual (==) (Left MoreThanOneRow)
--
-- --
-- -- getRecord
-- --
--
-- prop_getRecord_noRows :: Property
-- prop_getRecord_noRows = withTests 1 $ property $ do
--   actual <- testQuery $ Q.getRecord NoRows MoreThanOneRow (TestTableKey 0)
--   diff actual (==) (Left NoRows)
--
-- prop_getRecord_oneRow :: Property
-- prop_getRecord_oneRow = property $ do
--   xs@(x : _) <- forAll $ genUniqueList 1 10
--   actual <- testQueryWithInit
--     (populateTestTable xs)
--     (Q.getRecord NoRows MoreThanOneRow (TestTableKey x))
--   diff actual (==) (Right TestTable)
--
-- prop_getRecord_tooManyRows :: Property
-- prop_getRecord_tooManyRows = property $ do
--   xs@(x : _) <- forAll $ genUniqueList 1 10
--   -- Need a duplicate record to hit a MoreThanOneRow error
--   let dupeList = x : xs
--   actual <- testQueryWithInit
--     (populateTestTable dupeList)
--     (Q.getRecord NoRows MoreThanOneRow (TestTableKey x))
--   diff actual (==) (Left MoreThanOneRow)
--

tests :: IO Bool
tests =
  checkSequential $$discover
