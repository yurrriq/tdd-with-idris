-- ---------------------------------------------- [ DependentStateMachines.idr ]
-- Module      : Chapter.DependentStateMachines
-- Description : Definitions from Chapter 14 of Edwin Brady's book,
--               "Type-Driven Development with Idris."
-- --------------------------------------------------------------------- [ EOH ]
module Chapter.DependentStateMachines

import Data.Vect

-- ------------------------------------------ [ 14.1.1 Refining the Door Model ]

public export
data DoorResult = OK | Jammed

||| A door is either open or closed.
public export
data DoorState = ||| The door is closed.
                 DoorClosed
               | ||| The door is open.
                 DoorOpen

namespace Door

    public export
    data DoorCmd : (ty : Type) -> DoorState -> (ty -> DoorState) -> Type where
         Open : DoorCmd DoorResult DoorClosed
                                   (\res => case res of
                                                 OK     => DoorOpen
                                                 Jammed => DoorClosed)

         Close : DoorCmd () DoorOpen (const DoorClosed)
         RingBell : DoorCmd () DoorClosed (const DoorClosed)

         Display : String -> DoorCmd () state (const state)

         Pure : (res : ty) -> DoorCmd ty (state_fn res) state_fn
         (>>=) : DoorCmd a state1 state2_fn ->
               ((res : a) -> DoorCmd b (state2_fn res) state3_fn) ->
         DoorCmd b state1 state3_fn

logOpen : DoorCmd DoorResult DoorClosed
                             (\res => case res of
                                           OK     => DoorOpen
                                           Jammed => DoorClosed)
logOpen = do Display "Trying to open the door"
             OK <- Open | Jammed => do Display "Jammed"
                                       Pure Jammed
             Display "Success"
             Pure OK

-- -------------------------------------- [ 14.2.2 Defining a Type for the ATM ]

||| A PIN is exactly four characters.
PIN : Type
PIN = Vect 4 Char

||| The possible states of the ATM.
data ATMState = Ready | CardInserted | Session

||| The possible results of checking a PIN, valid or invalid.
data PINCheck = CorrectPIN | IncorrectPIN

data HasCard : ATMState -> Type where
     HasCI      : HasCard CardInserted
     HasSession : HasCard Session

namespace ATM

    data ATMCmd : (ty : Type) -> ATMState -> (ty -> ATMState) -> Type where
         InsertCard : ATMCmd ()   Ready        (const CardInserted)
         EjectCard  : {auto prf : HasCard state} ->
                      ATMCmd () state (const Ready)
         GetPIN     : ATMCmd PIN  CardInserted (const CardInserted)

         CheckPIN   : PIN -> ATMCmd PINCheck CardInserted
                               (\check => case check of
                                               CorrectPIN => Session
                                               IncorrectPIN => CardInserted)
         GetAmount : ATMCmd Nat state (const state)

         Dispense : (amount : Nat) -> ATMCmd () Session (const Session)

         Message : String -> ATMCmd () state (const state)
         Pure  : (res : ty) -> ATMCmd ty (state_fn res) state_fn
         (>>=) : ATMCmd a state1 state2_fn ->
                 ((res : a) -> ATMCmd b (state2_fn res) state3_fn) ->
                 ATMCmd b state1 state3_fn

atm : ATMCmd () Ready (const Ready)
atm = do InsertCard
         Message "Checking Card"
         case !(CheckPIN !GetPIN) of
              CorrectPIN   => do Dispense !GetAmount
                                 EjectCard
                                 Message "Please remove your card and cash"
              IncorrectPIN => do Message "Incorrect PIN"
                                 EjectCard

testPIN : Vect 4 Char
testPIN = ['1', '2', '3', '4']

readVect : (n : Nat) -> IO (Vect n Char)
readVect Z = do discard <- getLine
                pure []
readVect (S k) = do ch <- getChar
                    chs <- readVect k
                    pure (ch :: chs)

runATM : ATMCmd res inState outState_fn -> IO res
runATM InsertCard = do putStrLn "Please insert your card (press enter)"
                       x <- getLine
                       pure ()
runATM EjectCard = putStrLn "Card ejected"
runATM GetPIN = do putStr "Enter PIN: "
                   readVect 4
runATM (CheckPIN pin) = if pin == testPIN
                           then pure CorrectPIN
                           else pure IncorrectPIN
runATM GetAmount = do putStr "How much would you like? "
                      pure (cast !getLine)
runATM (Dispense amount) = putStrLn ("Here is " ++ show amount)

runATM (Message msg) = putStrLn msg
runATM (Pure res) = pure res
runATM (x >>= f) = do x' <- runATM x
                      runATM (f x')

-- --------------------------------------------------------------------- [ EOF ]
