-- --------------------------------------------------------- [ Concurrency.idr ]
-- Module      : Chapter.Concurrency
-- Description : Definitions from Chapter 15 of Edwin Brady's book,
--               "Type-Driven Development with Idris."
-- --------------------------------------------------------------------- [ EOH ]
module Chapter.Concurrency

import System.Concurrency.Channels
import Chapter.Concurrency.ProcessLib

-- import Control.Arrow
-- import Control.Category
-- import Data.Morphisms

-- ------------------------------------ [ 15.1.1 Defining Concurrent Processes ]

%access public export

data Message = Add Nat Nat

-- ---------------------------------------- [ 15.1.2 Primitive Message Passing ]

namespace Unsafe

  %access export

  partial adder :IO ()
  adder = do Just chan <- listen 1
               | Nothing => adder
             Just msg <- unsafeRecv Message chan
               | Nothing => adder
             case msg of
                  Add x y => do ok <- unsafeSend chan (x + y)
                                adder

  partial main : IO ()
  main = do Just pid <- spawn adder
              | Nothing => putStrLn "Spawn failed"
            Just chan <- connect pid
              | Nothing => putStrLn "Connection failed"
            ok <- unsafeSend chan (Add 2 3)
            Just answer <- unsafeRecv Nat chan
              | Nothing => putStrLn "Receive failed"
            printLn answer

-- ----------------------------------- [ 15.2.1 Describing Processes in a Type ]

namespace Safe

  %access public export

  data MessagePID = MkMessage PID

  data Process : Type -> Type where
       Request : MessagePID -> Message -> Process (Maybe Nat)
       Respond : ((msg : Message) -> Process Nat) -> Process (Maybe Message)
       Spawn   : Process () -> Process (Maybe MessagePID)
       Loop    : Inf (Process a) -> Process a

       Action  : IO a -> Process a
       Pure    : a -> Process a
       (>>=)   : Process a -> (a -> Process b) -> Process b

  run : Fuel -> Process t -> IO (Maybe t)
  run Dry _                  = pure Nothing
  run fuel (Action act)      = pure (Just !act)
  run fuel (Pure val)        = pure (Just val)
  run fuel (act >>= next)    = do Just x <- run fuel act
                                    | Nothing => pure Nothing
                                  run fuel (next x)
  run (More fuel) (Loop act) = run fuel act
  run fuel (Spawn proc)   = do Just pid <- spawn (run fuel proc *> pure ())
                                 | Nothing => pure Nothing
                               pure (Just (Just (MkMessage pid)))
  -- TODO: encapsulate timeout
  run fuel (Respond calc) = do Just sender <- listen 1
                                 | Nothing => pure (Just Nothing)
                               Just msg <- unsafeRecv Message sender
                                 | Nothing => pure (Just Nothing)
                               Just res <- run fuel (calc msg)
                                 | Nothing => pure Nothing
                               unsafeSend sender res
                               pure (Just (Just msg))
  run fuel (Request (MkMessage proc) msg) =
      do Just chan <- connect proc
           | Nothing => pure (Just Nothing)
         if !(unsafeSend chan msg)
            then do Just x <- unsafeRecv Nat chan
                      | Nothing => pure (Just Nothing)
                    pure (Just (Just x))
            else pure (Just Nothing)

  partial runProc : Process () -> IO ()
  runProc proc = run forever proc *> pure ()

  adder : Process ()
  adder = do Respond go
             Loop adder
    where
      go : Message -> Process Nat
      go (Add x y) = Pure (x + y)

  partial main : Process ()
  main = do Just pid <- Spawn adder
              | Nothing => Action (putStrLn "Spawn failed")
            Just answer <- Request pid (Add 2 3)
              | Nothing => Action (putStrLn "Request failed")
            Action (printLn answer)

  -- NOTE: These two definitions type-check, but don't respond to messages.

  -- adderBad1 : Process ()
  -- adderBad1 = do Action $ putStrLn "I'm out of the office today"
  --                    Loop adderBad1

  -- adderBad2 : Process ()
  -- adderBad2 = Loop adderBad2

namespace Stateful

  -- data AdderAction : Type where
  --      Add : Nat -> Nat -> AdderAction

  public export
  AdderType : Message -> Type
  AdderType (Add x y) = Nat

  export
  adder : Service AdderType ()
  adder = do Respond (\(Add x y) => Pure (x + y))
             Loop adder

namespace ListProcessing

  %access public export

  data ListAction : Type where
       Length : List elem -> ListAction
       Append : List elem -> List elem -> ListAction

  ListType : ListAction -> Type
  ListType (Length xs)           = Nat
  ListType (Append {elem} xs ys) = List elem

  %access export

  procList : Service ListType ()
  procList = do Respond go
                Loop procList
    where
      go : (msg : ListAction) -> Process ListType (ListType msg) Ready Ready
      go (Length xs)    = Pure (length xs)
      go (Append xs ys) = Pure (xs ++ ys)

  procMain : Client ()
  procMain = do Just list <- Spawn procList
                  | Nothing => Action (putStrLn "Spawn failed")
                len <- Request list (Length [1,2,3])
                Action (printLn len)
                app <- Request list (Append [1,2,3] [4,5,6])
                Action (printLn app)

namespace WordCounting

  %access public export

  record WCData where
      constructor MkWCData
      wordCount : Nat
      lineCount : Nat

  implementation Show WCData where
    show (MkWCData wc lc) = unlines [ "Words: " ++ show wc
                                    , "Lines: " ++ show lc
                                    ]

  data WC = CountFile String
          | GetData String

  implementation Show WC where
    show (CountFile fname) = "CountFile " ++ fname
    show (GetData fname) = "GetData " ++ fname

  WCType : WC -> Type
  WCType (CountFile _) = ()
  WCType (GetData _)   = Maybe WCData

  private
  doCount : (content : String) -> WCData
  doCount content = MkWCData (length (lines content))
                             (length (words content))

  -- doCount' : (content : String) -> WCData
  -- doCount' = applyMor $ arrow (length . lines) &&&
  --                       arrow (length . words) >>>
  --                       arrow (uncurry MkWCData)

  countFile : List (String, WCData) -> String ->
              Process WCType (List (String, WCData)) Sent Sent
  countFile files fname =
      do Right content <- Action (readFile fname)
           | Left _ => Pure files
         let count = doCount content
         Action (putStrLn ("Counting complete for " ++ fname))
         Pure ((fname, doCount content) :: files)

  wcService : (loaded : List (String, WCData)) ->
              Service WCType ()
  wcService loaded =
      do msg <- Respond go
         newLoaded <- case msg of
              Just (CountFile fname) => countFile loaded fname
              _                      => Pure loaded
         Loop (wcService newLoaded)
    where
      go : (msg : WC) -> Process WCType (WCType msg) Ready Ready
      go (CountFile fname) = Pure ()
      go (GetData fname)   = Pure (lookup fname loaded)

  procMain : Client ()
  procMain =
      do Just wc <- Spawn (wcService [])
           | Nothing => Action (putStrLn "Spawn failed")
         Action (putStrLn "Counting test.txt")
         let fname = "/Users/mohacker/src/yurrriq/tdd-with-idris/README.org"
         Request wc (CountFile fname)
         Action (putStrLn "Processing")
         Just wcdata <- Request wc (GetData fname)
           | Nothing => Action (putStrLn "File error")
         Action (printLn wcdata)

-- --------------------------------------------------------------------- [ EOF ]
