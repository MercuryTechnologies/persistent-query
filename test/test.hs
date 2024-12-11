import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import qualified Database.Persist.QueryTest

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  _results <- sequence [
       Database.Persist.QueryTest.tests
    -- , Test.Example.Confidence.tests
    -- , Test.Example.Coverage.tests
    -- , Test.Example.EvalNF.tests
    -- , Test.Example.Exception.tests
    -- , Test.Example.QuickCheck.tests
    -- , Test.Example.References.tests
    -- , Test.Example.Registry.tests
    -- , Test.Example.Resource.tests
    -- , Test.Example.Roundtrip.tests
    -- , Test.Example.STLC.tests
    ]

  --
  -- Normally we would exit with failure when tests fail using something like:
  --
  --   Control.Monad.unless (and results) $
  --     System.Exit.exitFailure
  --
  -- But this project is designed to actually show test errors as an example so
  -- we don't want it to break CI.
  --

  pure ()
