-- ------------------------------------------------------- [ StateMachines.idr ]
-- Module      : Exercises.StateMachines
-- Description : Solutions to the Chapter 13 exercises in Edwin Brady's
--               book, "Type-Driven Development with Idris."
-- --------------------------------------------------------------------- [ EOH ]
module Exercises.StateMachines

import public Chapter.StateMachines

%access export

-- -------------------------------------------------------- [ 13.1.5 Exercises ]

public export
data GuessCmd : Type -> Nat -> Nat -> Type where
     Try : Integer -> GuessCmd Ordering (S n) n

     Pure : ty -> GuessCmd ty state state
     (>>=) : GuessCmd a state1 state2 ->
             (a -> GuessCmd b state2 state3) ->
             GuessCmd b state1 state3

export
threeGuesses : GuessCmd () 3 0
threeGuesses = do Try 10
                  Try 20
                  Try 15
                  Pure ()

-- NOTE: noGuesses should not type check.
-- noGuesses : GuessCmd () 0 0
-- noGuesses = do Try 10
--                Pure ()

namespace Matter

  public export
  data Matter = Solid | Liquid | Gas

  public export
  data MatterCmd : (ty : Type) ->
                   (beforeState, afterState : Matter) ->
                   Type where
       Melt : MatterCmd () Solid Liquid
       Boil : MatterCmd () Liquid Gas
       Condense : MatterCmd () Gas Liquid
       Freeze : MatterCmd () Liquid Solid

       Pure : ty -> MatterCmd ty state state
       (>>=) : MatterCmd a state1 stat2 ->
               (a -> MatterCmd b state2 state3) ->
               MatterCmd b state1 state3

export
iceSteam : MatterCmd () Solid Gas
iceSteam = do Melt
              Boil

export
steamIce : MatterCmd () Gas Solid
steamIce = do Condense
              Freeze

-- NOTE: overMelt should not type check.
-- overMelt : MatterCmd () Solid Gas
-- overMelt = do Melt
--               Melt

-- --------------------------------------------------------------------- [ EOF ]
