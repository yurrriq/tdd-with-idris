-- ------------------------------------------------------- [ StateMachines.idr ]
-- Module      : Chapter.StateMachines
-- Description : Definitions from Chapter 13 of Edwin Brady's book,
--               "Type-Driven Development with Idris."
-- --------------------------------------------------------------------- [ EOH ]
module Chapter.StateMachines

import public Control.Monad.State

-- NOTE: See 13.2.1
import Data.Vect

%default total

-- ------------------------------------------------- [ 13.1.1 Modelling a Door ]

namespace BadDoor

  public export
  data DoorCmd : Type -> Type where
       Open : DoorCmd ()
       Close : DoorCmd ()
       RingBell : DoorCmd ()

       Pure : ty -> DoorCmd ty
       (>>=) : DoorCmd a -> (a -> DoorCmd b) -> DoorCmd b

  export
  doorProg : DoorCmd ()
  doorProg = do RingBell
                Open
                Close

  doorProgBad : DoorCmd ()
  doorProgBad = do Open
                   Open
                   RingBell

namespace Door

  ||| A door is either open or closed.
  public export
  data DoorState = ||| The door is closed.
                   DoorClosed
                 | ||| The door is open.
                   DoorOpen

  ||| A sequence of door actions which produces a value of type `ty`, begins with
  ||| the door in the state `beforeState`, and ends with the door in the state
  ||| `afterState`.
  ||| @ ty The type of the produced value.
  ||| @ beforeState The initial door state.
  ||| @ afterState The resulting door state.
  public export
  data DoorCmd  : (ty : Type) ->
                  (beforeState, afterState : DoorState) ->
                  Type where

       ||| Open a closed door.
       Open     : DoorCmd () DoorClosed DoorOpen

       ||| Close an open door.
       Close    : DoorCmd () DoorOpen   DoorClosed

       ||| Ring the bell of a closed door.
       RingBell : DoorCmd () DoorClosed DoorClosed

       ||| Lift a value.
       Pure : ty -> DoorCmd ty state state

       ||| Sequentially compose two door actions, passing any value produced by
       ||| the first as an argument to the second.
       (>>=) : DoorCmd a state1 state2 ->
               (a -> DoorCmd b state2 state3) ->
               DoorCmd b state1 state3

  ||| Starting with a closed door, ring the bell then open and close the door,
  ||| resulting in a closed door and producing ().
  export
  doorProg : DoorCmd () DoorClosed DoorClosed
  doorProg = do RingBell
                Open
                Close

-- -------------------------------------- [ 13.1.3 Modelling a Vending Machine ]

namespace VendingMachine

  export
  VendState : Type
  VendState = (Nat, Nat)

  public export
  data Input = COIN
             | VEND
             | CHANGE
             | REFILL Nat

  public export
  data MachineCmd : (ty : Type) ->
                    (beforeState, afterState : VendState) ->
                    Type where

       InsertCoin : MachineCmd () (  pounds,   chocs) (S pounds, chocs)

       Vend       : MachineCmd () (S pounds, S chocs) (  pounds, chocs)

       GetCoins   : MachineCmd () (  pounds,   chocs) (  Z,      chocs)

       Refill : (bars : Nat) -> MachineCmd () (Z, chocs) (Z, bars + chocs)

       Display : String -> MachineCmd () state state

       GetInput : MachineCmd (Maybe Input) state state

       ||| Lift a value.
       Pure : ty -> MachineCmd ty state state

       (>>=) : MachineCmd a state1 state2 ->
               (a -> MachineCmd b state2 state3) ->
               MachineCmd b state1 state3

  public export
  data MachineIO : VendState -> Type where
       Do : MachineCmd a state1 state2 ->
            (a -> Inf (MachineIO state2)) -> MachineIO state1

namespace MachineDo

  public export
  (>>=) : MachineCmd a state1 state2 ->
          (a -> Inf (MachineIO state2)) -> MachineIO state1
  (>>=) = Do

-- --------------------------------------- [ 13.1.4 A Verified Vending Machine ]

mutual

  vend : MachineIO (pounds, chocs)
  vend {pounds = S _} {chocs = S _}
       = do Vend
            Display "Enjoy!"
            machineLoop
  vend {pounds = Z}
       = do Display "Insert a coin"
            machineLoop
  vend {chocs = Z}
       = do Display "Out of stock"
            machineLoop

  refill : (num : Nat) -> MachineIO (pounds, chocs)
  refill {pounds = Z} num
         = do Refill num
              machineLoop
  refill _ = do Display "Can't refill: Coins in machine"
                machineLoop

  machineLoop : MachineIO (pounds, chocs)
  machineLoop = do Just x <- GetInput
                     | Nothing => do Display "Invalid input"
                                     machineLoop
                   case x of
                        COIN => do InsertCoin
                                   machineLoop
                        VEND => vend
                        CHANGE => do GetCoins
                                     Display "Change returned"
                                     machineLoop
                        REFILL num => refill num

-- ------------------------------------------------- [ 13.2.1 Stack Operations ]

namespace StackOperations

 public export
 data StackCmd : (ty : Type) ->
                 (a : Type) ->
                 (inHeight, outHeight : Nat) ->
                 Type where

      Push : a -> StackCmd () a height (S height)

      Pop : StackCmd a a (S height) height

      Top : StackCmd a a (S height) (S height)

      GetStr : StackCmd String a height height

      PutStr : String -> StackCmd () a height height

      Pure : ty -> StackCmd ty a height height
      (>>=) : StackCmd a elem height1 height2 ->
              (a -> StackCmd b elem height2 height3) ->
              StackCmd b elem height1 height3

-- export
testAdd : StackCmd Integer Integer 0 0
testAdd = do Push 10
             Push 20
             val1 <- Pop
             val2 <- Pop
             Pure (val1 + val2)

-- NOTE: This will not type check.
-- testAdd : StackCmd Integer 0 0
-- testAdd = do Push 10
--              val1 <- Pop
--              val2 <- Pop
--              Pure (val1 + val2)

-- ------------------------------------------- [ 13.2.2 Implementing the Stack ]

runStack : (stk : Vect inHeight a) ->
           StackCmd ty a inHeight outHeight ->
           IO (ty, Vect outHeight a)
runStack stk (Push val)   = pure ((), val :: stk)
runStack (val :: stk) Pop = pure (val, stk)
runStack (val :: stk) Top = pure (val, val :: stk)
runStack stk GetStr       = do x <- getLine
                               pure (x, stk)
runStack stk (PutStr x)   = putStr x *> pure ((), stk)
runStack stk (Pure x)     = pure (x, stk)
runStack stk (x >>= f)    = do (x', newStk) <- runStack stk x
                               runStack newStk (f x')

doAdd : StackCmd () Integer (S (S height)) (S height)
doAdd = do val1 <- Pop
           val2 <- Pop
           Push (val1 + val2)

public export
data StackIO : Nat -> Type -> Type where
     Do : StackCmd ty a inHeight outHeight ->
          (ty -> Inf (StackIO outHeight a)) ->
          StackIO inHeight a

namespace StackDo

  (>>=) : StackCmd ty a inHeight outHeight ->
          (ty -> Inf (StackIO outHeight a)) ->
          StackIO inHeight a
  (>>=) = Do

public export
data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

export
run : Fuel -> Vect height a -> StackIO height a -> IO ()
run (More fuel) stk (Do c f) = do (res, newStk) <- runStack stk c
                                  run fuel newStk (f res)
run Dry _ _                  = pure ()

public export
data StkInput = Number Integer
              | Add

export
strToInput : String -> Maybe StkInput
strToInput "" = Nothing
strToInput "+" = Just Add
strToInput "-" = Just Subtract
strToInput "*" = Just Multiply
strToInput x = if all isDigit (unpack x)
                  then Just (Number (cast x))
                  else Nothing

mutual

  binOp : StackIO height Integer

  tryAdd : StackIO height Integer
  tryAdd {height = S (S h)}
         = do doAdd
              result <- Top
              PutStr (show result ++ "\n")
              stackCalc
  tryAdd = do PutStr "Fewer than two items on the stack\n"
              stackCalc

-- -----------------------------------------------------------------------------

  export
  stackCalc : StackIO height Integer
  stackCalc = do PutStr "> "
                 input <- GetStr
                 case strToInput input of
                      Nothing => do PutStr "Invalid input\n"
                                    stackCalc
                      Just (Number x) => do Push x
                                            stackCalc
                      Just Add => tryAdd
                      -- NOTE: Exercise 13.2.4.1
                      Just Subtract => ?trySubtract
                      Just Multiply => ?tryMultiply

namespace StackCalculator

  export
  partial
  main : IO ()
  main = run forever [] stackCalc

-- --------------------------------------------------------------------- [ EOF ]
