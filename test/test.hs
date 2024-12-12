import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import qualified Database.Persist.Query.WithErrorTest
import Hedgehog.Main

main :: IO ()
main = defaultMain
       [ Database.Persist.Query.WithErrorTest.tests
       ]
