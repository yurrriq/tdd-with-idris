-- --------------------------------------------------------------- [ State.idr ]
-- Module      : Exercises.Test.State
-- Description : Test covering the Chapter 12 exercises in Edwin Brady's
--               book, "Type-Driven Development with Idris."
-- --------------------------------------------------------------------- [ EOH ]
module Exercises.Test.State

import Exercises.State

import Test.Helpers

import Control.Monad.State
import System

import Data.Vect
%hide Data.Vect.overLength

%access export

testIncrease : IO Bool
testIncrease =
  "increase" `assertEqual'` ((), 94) $
  runState (increase 5) 89

testCountEmpty : IO Bool
testCountEmpty =
  "countEmpty" `assertEqual'` 7 $
  execState (countEmpty testTree) 0

testCountEmptyNode : IO Bool
testCountEmptyNode =
  "countEmptyNode" `assertEqual'` (7, 6) $
  execState (countEmptyNode testTree) (0, 0)

testUpdateGameState : IO Bool
testUpdateGameState =
  do let command = updateGameState addCorrect
     rnds <- pure (randoms . fromInteger) <*> time
     (_, _, st) <- runCommand rnds initState command
     "updateGameState" `assertEqual'` 1 $
       record { score->correct } st

goodSite : Article
goodSite = MkArticle "Good Page" "http://example.com/good" (MkVotes 101 7)

badSite : Article
badSite = MkArticle "Bad Page" "http://example.com/bad" (MkVotes 5 47)

testGetScore : (article : Article) -> (expected : Integer) -> IO Bool
testGetScore article expected =
  ("getScore " ++ title article) `assertEqual'` expected $
  getScore article

testAddUpvote : IO Bool
testAddUpvote =
  "addUpvote" `assertEqual'` 102 $
  record { score->upvotes } $ addUpvote goodSite

testAddDownvote : IO Bool
testAddDownvote =
  "addDownvote" `assertEqual'` 48 $
  record { score->downvotes } $ addDownvote badSite

allTests : List (IO Bool)
allTests = [ testIncrease
           , testCountEmpty
           , testCountEmptyNode
           , testUpdateGameState
           , testGetScore goodSite 94
           , testGetScore badSite (-42)
           , testAddUpvote
           , testAddDownvote
           ]

runTests : IO (Vect (length State.allTests) Bool)
runTests = testChapter "12: State" allTests

-- --------------------------------------------------------------------- [ EOF ]
