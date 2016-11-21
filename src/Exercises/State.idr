-- --------------------------------------------------------------- [ State.idr ]
-- Module      : Exercises.State
-- Description : Solutions to the Chapter 12 exercises in Edwin Brady's
--               book, "Type-Driven Development with Idris."
-- --------------------------------------------------------------------- [ EOH ]
module Exercises.State

import public Chapter.State

%access export

-- -------------------------------------------------------- [ 12.1.5 Exercises ]

update : (stateType -> stateType) -> State stateType ()
update = (get >>=) . (put .)

increase : Nat -> State Nat ()
increase = update . (+)

%hide Chapter.State.increase

countEmpty : Tree a -> State Nat ()
countEmpty  Empty              = increase 1
countEmpty (Node left _ right) = countEmpty left *> countEmpty right

-- Like Bifunctor.first for Pair.
private
first : (a -> c) -> (a, b) -> (c, b)
first f (a, b) = (f a, b)

-- Like Bifunctor.second for Pair.
private
second : (b -> d) -> (a, b) -> (a, d)
second g (a, b) = (a, g b)

countEmptyNode : Tree a -> State (Nat, Nat) ()
countEmptyNode Empty               = update (first S)
countEmptyNode (Node left _ right) = countEmptyNode left  *>
                                     countEmptyNode right *>
                                     update (second S)

-- -------------------------------------------------------- [ 12.3.7 Exercises ]

export
updateGameState : (GameState -> GameState) -> Command ()
updateGameState f = GetGameState >>= PutGameState . f

public export
implementation Functor Command where
  map f x = Bind x (Pure . f)

public export
implementation Applicative Command where
  pure = Pure
  f <*> m = Bind f $ (Bind m) . (Pure .)

public export
implementation Monad Command where
  (>>=) = Bind

public export
record Votes where
  constructor MkVotes
  upvotes, downvotes : Integer

public export
record Article where
  constructor MkArticle
  title, url : String
  score : Votes

export
initPage : (title, url : String) -> Article
initPage title url = MkArticle title url (MkVotes 0 0)

export
getScore : Article -> Integer
getScore = (\(MkVotes up down) => up - down) . score

export
addUpvote : Article -> Article
addUpvote = record { score->upvotes $= (+1) }

export
addDownvote : Article -> Article
addDownvote = record { score->downvotes $= (+1) }

-- --------------------------------------------------------------------- [ EOF ]
