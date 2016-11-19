module Chapter.State

-- NOTE: See 12.1.3
import public Control.Monad.State

%default total

-- ----------------------------------------- [ 12.1 Working with Mutable State ]

||| Attach labels from `lbls` to elements of `vals`.
||| @ lbls A stream of labels.
||| @ vals A list of values.
export
labelWith : (lbls : Stream labelType) -> (vals : List a) -> List (labelType, a)
labelWith lbls [] = []
labelWith (lbl :: lbls) (val :: vals) = (lbl, val) :: labelWith lbls vals

-- ---------------------------------- [ 12.1.1 Running Example: Tree Traversal ]

public export
data Tree a = Empty
            | Node (Tree a) a (Tree a)

public export
testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

export
flatten : Tree a -> List a
flatten Empty = []
flatten (Node left val right) = flatten left ++ val :: flatten right

-- -------------------------- [ 12.1.2 Representing Mutable State using a Pair ]

export
treeLabelWith : Stream labelType -> Tree a ->
                (Stream labelType, Tree (labelType, a))
treeLabelWith lbls Empty = (lbls, Empty)
treeLabelWith lbls (Node left val right)
  = let (this :: lblsLeft, leftLabelled) = treeLabelWith lbls left
        (lblsRight, rightLabelled) = treeLabelWith lblsLeft right in
        (lblsRight, Node leftLabelled (this, val) rightLabelled)

export
treeLabel : Tree a -> Tree (Integer, a)
treeLabel tree = snd (treeLabelWith [1..] tree)

-- ----------------- [ 12.1.3 State, a Type for Describing Stateful Operations ]

export
increase : Nat -> State Nat ()
increase inc = do current <- get
                  put (current + inc)

-- NOTE:
-- runState : StateT stateType Identity a -> stateType -> (a, stateType)
-- evalState : State stateType a -> stateType -> a
-- execState : State stateType a -> stateType -> stateType
-- get : MonadState stateType m => m stateType
-- put : MonadState stateType m => stateType -> m ()

-- ---------------------------------------- [ 12.1.4 Tree Traversal with State ]

namespace Stateful
  export
  treeLabelWith : Tree a -> State (Stream labelType) (Tree (labelType, a))
  treeLabelWith Empty = pure Empty
  treeLabelWith (Node left val right)
    = do leftLabelled <- treeLabelWith left
         -- Get the current stream of labels.
         (this :: rest) <- get
         -- Set the new stream of labels to be
         -- the tail of the current stream, rest.
         put rest
         rightLabelled <- treeLabelWith right
         pure (Node leftLabelled (this, val) rightLabelled)

  export
  treeLabel : Tree a -> Tree (Integer, a)
  treeLabel tree = evalState (treeLabelWith tree) [1..]

-- --------------------------------------------------------------------- [ EOF ]
