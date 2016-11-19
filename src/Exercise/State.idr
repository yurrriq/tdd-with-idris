module Exercise.State

import public Chapter.State

%default total
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

-- --------------------------------------------------------------------- [ EOF ]
