{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}


module Database.Persist.Query.CommonTest (tests) where

import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty qualified as NE
import Database.Persist.Query.WithError qualified as Q
import GHC.Stack
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

tests :: IO Bool
tests = do
  checkSequential $$discover
