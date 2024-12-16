{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Database.Persist.Query.CommonCallStackTest (tests) where

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
-- Test that the module functions return the stack we expect.
--

-- unexpected, called at test/Database/Persist/Query/CommonTest.hs:146:47
prop_cstack_unexpected :: Property
prop_cstack_unexpected = callStackTesterAsProperty (Q.unexpected "") [1]

-- single, called at test/Database/Persist/Query/CommonTest.hs:159:4
prop_cstack_single :: Property
prop_cstack_single = callStackTesterAsProperty (Q.single "" "" []) [1]

-- atLeastOne, called at test/Database/Persist/Query/CommonTest.hs:163:4
prop_cstack_atLeastOne :: Property
prop_cstack_atLeastOne = callStackTesterAsProperty (Q.atLeastOne "" []) [1]

-- oneOrNone, called at test/Database/Persist/Query/CommonTest.hs:168:46
prop_cstack_oneOrNone :: Property
prop_cstack_oneOrNone = callStackTesterAsProperty (Q.oneOrNone "" [1, 2]) [1]

-- getRecord, called at test/Database/Persist/Query/CommonTest.hs:171:46
prop_cstack_getRecord :: Property
prop_cstack_getRecord = callStackTesterAsProperty (Q.getRecord "" "" (TestTableKey 0)) [1]

--
-- Tests that the we get the stack traces we expect.
-- Each test lists a sample QueryStackTrace that it uses.
--

-- for n=0
-- single, called at test/Database/Persist/Query/CommonTest.hs:151:19
-- callStackTest, called at test/Database/Persist/Query/CommonTest.hs:175:43
prop_cstack_nested :: Property
prop_cstack_nested = withTests 10 $ property $ do
  n <- forAll $ Gen.int (Range.linear 0 5) -- how many recursive calls
  callStackTester (callStackTest n) [n + 2]

-- for n=0
--  CallStack (from HasCallStack):
--    single, called at test/Database/Persist/Query/CommonTest.hs:155:19
--    callStackTest, called at test/Database/Persist/Query/CommonTest.hs:152:45
--    callStackTestWrapper, called at test/Database/Persist/Query/CommonTest.hs:149:31
--    callStackTestWrapperWrapper, called at test/Database/Persist/Query/CommonTest.hs:189:24
--  CallStack (from HasCallStack):
--    queryCheckpoint, called at test/Database/Persist/Query/CommonTest.hs:152:26
--    callStackTestWrapper, called at test/Database/Persist/Query/CommonTest.hs:149:31
--    callStackTestWrapperWrapper, called at test/Database/Persist/Query/CommonTest.hs:189:24
prop_cstack_queryCheckpoint :: Property
prop_cstack_queryCheckpoint = withTests 10 $ property $ do
  n <- forAll $ Gen.int (Range.linear 0 5) -- how many recursive calls
  callStackTester (callStackTestWrapperWrapper n) [n+3, n+4]

-- | Test that we get the tops of stack traces as expected
prop_cstack_stackTraceTops :: Property
prop_cstack_stackTraceTops = withTests 10 $ property $ do
  n <- forAll $ Gen.int (Range.linear 0 5) -- how many recursive calls
  Left (queryStackTrace, _) <- testQueryWithStackTrace $ callStackTestWrapperWrapper n
  -- shows the stack traces on failure
  mapM_ (footnote . prettyCallStack) queryStackTrace
  -- get the function names
  let actual = map fst (getCallStack $ Q.stackTraceTops queryStackTrace)
  diff actual (==) [ "callStackTestWrapper" , "callStackTest" ]

--
-- Helpers
--

-- | Determines the lengths of each CallStack. Easy to check if the traces are the length we expect.
callStackLengths :: Q.QueryStackTrace -> [Int]
callStackLengths = map (length . getCallStack)

-- | Run a query, which should error out, and check if the QueryStackTrace is the size we expect.
callStackTester :: Q.Query e t -> [Int] -> PropertyT IO ()
callStackTester q expected = do
  result <- testQueryWithStackTrace q
  case result of
    Right _ -> failure
    Left (cStack, _) -> do
      mapM_ (footnote . prettyCallStack) cStack
      diff (callStackLengths cStack) (==) expected

-- | Run a callStackTester as a Property
callStackTesterAsProperty :: Q.Query e t -> [Int] -> Property
callStackTesterAsProperty q expected = withTests 1 $ property $ callStackTester q expected

--
-- Functions to help build CallStacks for testing
--

callStackTest :: HasCallStack => Int -> Q.Query String Int
callStackTest 0 = Q.single "" "" []
callStackTest n = callStackTest (n - 1)

callStackTestWrapper :: HasCallStack => Int -> Q.Query String Int
callStackTestWrapper n = Q.queryCheckpoint (callStackTest n)

callStackTestWrapperWrapper :: HasCallStack => Int -> Q.Query String Int
callStackTestWrapperWrapper = callStackTestWrapper

--
-- Discover tests
--

tests :: IO Bool
tests = do
  checkSequential $$discover
