-- --------------------------------------------------------------- [ State.idr ]
-- Module      : Chapter.State
-- Description : Definitions from Chapter 12 of Edwin Brady's book,
--               "Type-Driven Development with Idris."
-- --------------------------------------------------------------------- [ EOH ]
module Chapter.State

-- NOTE: See 12.1.3
import public Control.Monad.State

-- NOTE: See 12.3.6
import Data.Primitives.Views

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

-- ----------------------------------------------------------- [ Listing 12.13 ]

export
addIfPositive : Integer -> State Integer Bool
addIfPositive val = do let positive = val > 0
                       when positive $ update (+ val)
                       pure positive
  where
    update = (get >>=) . (put .)

export
addPositives : List Integer -> State Integer Nat
addPositives vals = do added <- traverse addIfPositive vals
                       pure (length (filter id added))

-- ---------------------------------------------------- [ 12.3.2 Complex State ]

public export
record Score where
       constructor MkScore
       correct, attempted : Nat

public export
record GameState where
       constructor MkGameState
       score : Score
       difficulty : Int

public export
implementation Show GameState where
  show st = show (record { score->correct } st) ++ " / " ++
            show (record { score->attempted } st) ++ "\n" ++
            "Difficulty: " ++ show (difficulty st)

export
initState : GameState
initState = MkGameState (MkScore 0 0) 12

-- ------------------------------------- [ 12.3.1 The Arithmetic Quiz Revisted ]

public export
data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String

     GetRandom : Command Int

     GetGameState : Command GameState

     PutGameState : GameState -> Command ()

     Pure : ty -> Command ty
     Bind : Command a -> (a -> Command b) -> Command b

public export
data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace CommandDo

  public export
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

namespace ConsoleDo

  public export
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) =  Do

public export
data Fuel = Dry | More (Lazy Fuel)

public export
partial
forever : Fuel
forever = More forever

-- ----------------------------------------------- [ 12.3.6 Executing the Quiz ]

export
runCommand : Stream Int ->
             GameState ->
             Command a ->
             IO (a, Stream Int, GameState)
runCommand rnds state (PutStr x) = do putStr x
                                      pure ((), rnds, state)
runCommand rnds state GetLine    = do str <- getLine
                                      pure (str, rnds, state)

runCommand (val :: rnds) state GetRandom =
  pure (getRandom val (difficulty state), rnds, state)
  where
    getRandom : Int -> Int -> Int
    getRandom val max with (divides val max)
      getRandom val 0             | DivByZero = 1
      getRandom ((_ * _) + rem) _ | DivBy prf = abs rem + 1

runCommand rnds state GetGameState            = pure (state, rnds, state)
runCommand rnds state (PutGameState newState) = pure ((), rnds, newState)

runCommand rnds state (Pure val) = pure (val, rnds, state)
runCommand rnds state (Bind c f) =
  do (res, newRnds, newState) <- runCommand rnds state c
     runCommand newRnds newState (f res)

export
run : Fuel -> Stream Int -> GameState -> ConsoleIO a ->
      IO (Maybe a, Stream Int, GameState)
run fuel rnds state (Quit val) = do pure (Just val, rnds, state)
run (More fuel) rnds state (Do cmd f) =
  do (res, newRnds, newState) <- runCommand rnds state cmd
     run fuel newRnds newState (f res)
run Dry rnds state _ = pure (Nothing, rnds, state)

-- ------------------------------------- [ 12.3.3 Updating Record Field Values ]

export
setDifficulty : Int -> GameState -> GameState
setDifficulty newDiff = record { difficulty = newDiff}

export
addWrong : GameState -> GameState
addWrong = record { score->attempted $= (+1) }

export
addCorrect : GameState -> GameState
addCorrect = record { score->correct   $= (+1)
                    , score->attempted $= (+1)
                    }

-- -------------------------------------------- [ 12.3.5 Implementing the Quiz ]

-- Copied from 11.3.3
public export
data Input = Answer Int
           | QuitCmd

mutual
  export
  correct : ConsoleIO GameState
  correct = do PutStr "Correct!\n"
               st <- GetGameState
               PutGameState (addCorrect st)
               quiz
  export
  wrong : Int -> ConsoleIO GameState
  wrong ans = do PutStr $ "Wrong, the answer is " ++ show ans ++ "\n"
                 st <- GetGameState
                 PutGameState (addWrong st)
                 quiz
  export
  readInput : (prompt : String) -> Command Input
  readInput prompt = do PutStr prompt
                        answer <- GetLine
                        if toLower answer == "quit"
                           then Pure QuitCmd
                           else Pure (Answer (cast answer))
  export
  quiz : ConsoleIO GameState
  quiz = do x <- GetRandom
            y <- GetRandom
            st <- GetGameState
            PutStr $ show st ++ "\n"
            let solution = x * y
            QuitCmd <- readInput $ show x ++ " * " ++ show y ++ "? "
              | Answer answer => if solution == answer
                                    then correct
                                    else wrong solution
            Quit st

-- ----------------------------------------------------------- [ Listing 12.32 ]

export
randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

-- --------------------------------------------------------------------- [ EOF ]
