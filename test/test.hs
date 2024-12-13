import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import qualified Database.Persist.Query.CommonTest
import qualified Database.Persist.Query.WithErrorTest
import Hedgehog.Main

main :: IO ()
main = defaultMain
       [ Database.Persist.Query.CommonTest.tests
       , Database.Persist.Query.WithErrorTest.tests
       ]
