-- ---------------------------------------------------------------- [ Main.idr ]
-- Module      : Main
-- Description : Test runner for solutions to exercises in Edwin Brady's
--               book, "Type-Driven Development with Idris."
-- --------------------------------------------------------------------- [ EOH ]

module Main

import Exercises.Test.GettingStarted
import Exercises.Test.Interactive
import Exercises.Test.DataTypes
import Exercises.Test.State

import Test.Helpers
import Test.Utils

import Data.Vect
import System

main : IO ()
main = do resCh2  <- GettingStarted.runTests
          resCh3  <- Interactive.runTests
          resCh4  <- DataTypes.runTests
          resCh12 <- State.runTests
          let results = resCh2 ++ resCh3 ++ resCh4 ++ resCh12
          printSummary results 
          when (any not results) (exit 1)

-- --------------------------------------------------------------------- [ EOF ]
