-- ----------------------------------------------------------- [ DataTypes.idr ]
-- Module      : Exercises.Test.DataTypes
-- Description : Test covering the Chapter 4 exercises in Edwin Brady's
--               book, "Type-Driven Development with Idris."
-- --------------------------------------------------------------------- [ EOH ]
module Exercises.Test.DataTypes

import Exercises.DataTypes
import Test.Helpers

%access export

implementation Eq (Tree Integer) where
  Empty == Empty                     = True
  (Node l1 x1 r1) == (Node l2 x2 r2) = x1 == x2 && l1 == l2 && r1 == r2
  _ == _                             = False

implementation Show (Tree Integer) where
  show t = "#<Tree>" -- FIXME

testListToTree : IO Bool
testListToTree =
  "listToTree" `assertEqual` Node (Node Empty 1 Empty)
                                  2
                                  (Node (Node Empty 3 (Node Empty 4 Empty))
                                        5
                                        Empty) $
  listToTree [1,4,3,5,2]

testTreeToList : IO Bool
testTreeToList =
  "treeToList" `assertEqual` [1..9] $
  treeToList (listToTree [4,1,8,7,2,3,9,5,6])

runTest : IO ()
runTest = do putStrLn "Testing Chapter 4: User Defined Data Types"
             putStrLn infoLine
             runTests [ testListToTree
                      , testTreeToList
                      ]

-- --------------------------------------------------------------------- [ EOF ]
