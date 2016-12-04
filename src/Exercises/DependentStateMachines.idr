-- ---------------------------------------------- [ DependentStateMachines.idr ]
-- Module      : Exercises.DependentStateMachines
-- Description : Solutions to the Chapter 14 exercises in Edwin Brady's
--               book, "Type-Driven Development with Idris."
-- --------------------------------------------------------------------- [ EOH ]
module Exercises.DependentStateMachines

import public Chapter.DependentStateMachines

-- -------------------------------------------------------- [ 14.2.5 Exercises ]

%access public export

data Access = LoggedOut | LoggedIn
data PwdCheck = Correct | Incorrect

data ShellCmd  : (ty : Type) -> Access -> (ty -> Access) -> Type where
     Password  : String -> ShellCmd PwdCheck LoggedOut
                                             (\res => case res of
                                                           Correct => LoggedIn
                                                           Incorrect => LoggedOut)
     Logout    : ShellCmd () LoggedIn (const LoggedOut)
     GetSecret : ShellCmd String LoggedIn (const LoggedIn)

     PutStr    : String -> ShellCmd () state (const state)
     Pure      : (res : ty) -> ShellCmd ty (state_fn res) state_fn
     (>>=)     : ShellCmd a state1 state2_fn ->
                 ((res : a) -> ShellCmd b (state2_fn res) state3_fn) ->
                 ShellCmd b state1 state3_fn

%access export

session : ShellCmd () LoggedOut (const LoggedOut)
session = do Correct <- Password "wurzel"
               | Incorrect => PutStr "Wrong password"
             PutStr $ "Secret code: " ++ show !GetSecret ++ "\n"
             Logout

-- NOTE: The following should not type check.
{-
bad_session : ShellCmd () LoggedOut (const LoggedOut)
bad_session = do Password "wurzel"
                 PutStr $ "Secret code: " ++ !GetSecret ++ "\n"
                 Logout

no_logout : ShellCmd () LoggedOut (const LoggedOut)
no_logout = do Correct <- Password "wurzel"
                 | Incorrect => PutStr "Wrong password"
               PutStr $ "Secret code: " ++ show !GetSecret ++ "\n"
-}

namespace DependentVendingMachine

    export
    VendState : Type
    VendState = (Nat, Nat)

    public export
    data Input = COIN
               | VEND
               | CHANGE
               | REFILL Nat

    data CoinResult = Inserted | Rejected

    public export
    data MachineCmd : (ty : Type) -> VendState -> (ty -> VendState) -> Type where

         InsertCoin : MachineCmd CoinResult (pounds, chocs)
                                 (\res => case res of
                                               Inserted => (S pounds, chocs)
                                               Rejected => (pounds, chocs))

         Vend : MachineCmd () (S pounds, S chocs) (const (pounds, chocs))

         GetCoins : MachineCmd () (pounds, chocs) (const (Z, chocs))

         Refill : (bars : Nat) -> MachineCmd () (Z, chocs)
                                             (const (Z, bars + chocs))

         Display : String -> MachineCmd () state (const state)

         GetInput : MachineCmd (Maybe Input) state (const state)

         Pure : (res : ty) -> MachineCmd ty (state_fn res) state_fn

         (>>=) : MachineCmd a state1 state2_fn ->
                 ((res : a) -> MachineCmd b (state2_fn res) state3_fn) ->
                 MachineCmd b state1 state3_fn

    public export
    data MachineIO : VendState -> Type where
         Do : MachineCmd a state1 state2_fn ->
              ((res : a) -> Inf (MachineIO (state2_fn res))) -> MachineIO state1

namespace MachineDo

    public export
    (>>=) : MachineCmd a state1 state2_fn ->
            ((res : a) -> Inf (MachineIO (state2_fn res))) -> MachineIO state1
    (>>=) = Do

-- --------------------------------------- [ 13.1.4 A Verified Vending Machine ]

mutual

  vend : MachineIO (pounds, chocs)
  vend {pounds = S _} {chocs = S _} = do Vend
                                         Display "Enjoy!"
                                         machineLoop
  vend {pounds = Z}                 = do Display "Insert a coin"
                                         machineLoop
  vend {chocs = Z}                  = do Display "Out of stock"
                                         machineLoop

  refill : (num : Nat) -> MachineIO (pounds, chocs)
  refill {pounds = Z} num = do Refill num
                               machineLoop
  refill _                = do Display "Can't refill: Coins in machine"
                               machineLoop

  machineLoop : MachineIO (pounds, chocs)
  machineLoop =
      do Just x <- GetInput
           | Nothing => do Display "Invalid input"
                           machineLoop
         case x of
              COIN       => do Inserted <- InsertCoin
                                 | Rejected => do Display "Coin rejected"
                                                  machineLoop
                               Display "Coin inserted"
                               machineLoop
              VEND       => vend
              CHANGE     => do GetCoins
                               Display "Change returned"
                               machineLoop
              REFILL num => refill num

-- --------------------------------------------------------------------- [ EOF ]
