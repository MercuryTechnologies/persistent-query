{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}


module Database.Persist.Query.WithErrorTest (tests) where

import Data.List.NonEmpty qualified as NE
import Database.Persist.Query.WithError qualified as Q
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Models
import Test.Utils

--
-- single
--

-- Empty list returns @Left NoRows@
prop_single_noRows :: Property
prop_single_noRows = withTests 1 $ property $ do
  actual <- testQuery $ Q.single NoRows MoreThanOneRow emptyList
  diff actual (==) (Left NoRows)

prop_single_oneRow :: Property
prop_single_oneRow = property $ do
  x <- forAll genListElement
  actual <- testQuery $ Q.single NoRows MoreThanOneRow [x]
  diff actual (==) (Right x)

--  List with more than one element returns @Left MoreThanOneRow@
prop_single_tooManyRows :: Property
prop_single_tooManyRows = property $ do
  xs <- forAll $ genList 2 10
  actual <- testQuery $ Q.single NoRows MoreThanOneRow xs
  diff actual (==) (Left MoreThanOneRow)

--
-- atLeastOne
--

prop_atLeastOne_noRows :: Property
prop_atLeastOne_noRows = withTests 1 $ property $ do
  actual <- testQuery $ Q.atLeastOne NoRows emptyList
  diff actual (==) (Left NoRows)

prop_atLeastOne_oneOrMoreRows :: Property
prop_atLeastOne_oneOrMoreRows = property $ do
  xs  <- forAll $ genList 1 10
  actual <- testQuery $ Q.atLeastOne NoRows xs
  diff actual (==) (Right $ NE.fromList xs)

--
-- firstOfMany
--

prop_firstOfMany_noRows :: Property
prop_firstOfMany_noRows = withTests 1 $ property $ do
  actual <- testQuery $ Q.firstOfMany NoRows emptyList
  diff actual (==) (Left NoRows)

prop_firstOfMany_oneOrMoreRows :: Property
prop_firstOfMany_oneOrMoreRows = property $ do
  xs@(x : _)  <- forAll $ genList 1 10
  actual <- testQuery $ Q.firstOfMany NoRows xs
  diff actual (==) (Right x)

--
-- oneOrNone
--

prop_oneOrNone_noRows :: Property
prop_oneOrNone_noRows = withTests 1 $ property $ do
  actual <- testQuery $ Q.oneOrNone MoreThanOneRow emptyList
  diff actual (==) (Right Nothing)

prop_oneOrNone_oneRow :: Property
prop_oneOrNone_oneRow = property $ do
  x <- forAll genListElement
  actual <- testQuery $ Q.oneOrNone MoreThanOneRow [x]
  diff actual (==) (Right $ Just x)

--  List with more than one element returns @Left MoreThanOneRow@
prop_oneOrNone_tooManyRows :: Property
prop_oneOrNone_tooManyRows = property $ do
  xs <- forAll $ genList 2 10
  actual <- testQuery $ Q.oneOrNone MoreThanOneRow xs
  diff actual (==) (Left MoreThanOneRow)

--
-- getRecord
--

prop_getRecord_noRows :: Property
prop_getRecord_noRows = withTests 1 $ property $ do
  actual <- testQuery $ Q.getRecord NoRows MoreThanOneRow (TestTableKey 0)
  diff actual (==) (Left NoRows)

prop_getRecord_oneRow :: Property
prop_getRecord_oneRow = property $ do
  xs@(x : _) <- forAll $ genUniqueList 1 10
  actual <- testQueryWithInit
    (populateTestTable xs)
    (Q.getRecord NoRows MoreThanOneRow (TestTableKey x))
  diff actual (==) (Right TestTable)

prop_getRecord_tooManyRows :: Property
prop_getRecord_tooManyRows = property $ do
  xs@(x : _) <- forAll $ genUniqueList 1 10
  -- Need a duplicate record to hit a MoreThanOneRow error
  let dupeList = x : xs
  actual <- testQueryWithInit
    (populateTestTable dupeList)
    (Q.getRecord NoRows MoreThanOneRow (TestTableKey x))
  diff actual (==) (Left MoreThanOneRow)



tests :: IO Bool
tests =
  checkSequential $$discover
