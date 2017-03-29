module Chapter.Concurrency.ProcessLib

import System.Concurrency.Channels

export
data MessagePID : (iface : reqType -> Type) -> Type where
     MkMessage : PID -> MessagePID iface

%access public export

NoRecv : Void -> Type
NoRecv = const Void

data ProcState = Ready
               | Sent
               | Looping

data Process : (iface : reqType -> Type) ->
               Type ->
               (in_state  : ProcState) ->
               (out_state : ProcState) ->
               Type where
     Request : MessagePID service_iface ->
               (msg : service_reqType) ->
               Process ifae (service_iface msg) st st
     Respond : ((msg : reqType) -> Process iface (iface msg) Ready Ready) ->
               Process iface (Maybe reqType) st Sent
     Spawn   : Process service_iface () Ready Looping ->
               Process iface (Maybe (MessagePID service_iface)) st st

     Loop    : Inf (Process iface a Ready Looping) ->
               Process iface a Sent Looping
     Action  : IO a -> Process iface a st st
     Pure    : a -> Process iface a st st
     (>>=)   : Process iface a st1 st2 ->
               (a -> Process iface b st2 st3) ->
               Process iface b st1 st3

data Fuel = Dry
          | More (Lazy Fuel)

%access export

partial
forever : Fuel
forever = More forever

run : Fuel -> Process iface t in_state out_state -> IO (Maybe t)
run fuel (Request {service_iface} (MkMessage proc) msg) =
    do Just chan <- connect proc
         | Nothing => pure Nothing
       if !(unsafeSend chan msg)
          then do Just x <- unsafeRecv (service_iface msg) chan
                    | Nothing => pure Nothing
                  pure (Just x)
          else pure Nothing
-- TODO: encapsulate timeout
run fuel (Respond {reqType} calc) =
    do Just sender <- listen 1
         | Nothing => pure (Just Nothing)
       Just msg <- unsafeRecv reqType sender
         | Nothing => pure (Just Nothing)
       Just res <- run fuel (calc msg)
         | Nothing => pure Nothing
       unsafeSend sender res
       pure (Just (Just msg))
run fuel (Spawn proc)       = do Just pid <- spawn (run fuel proc *> pure ())
                                   | Nothing => pure (Just Nothing)
                                 pure (Just (Just (MkMessage pid)))
run (More fuel) (Loop proc) = run fuel proc
run fuel (Action act)       = pure (Just !act)
run fuel (Pure val)         = pure (Just val)
run fuel (act >>= next)     = do Just x <- run fuel act
                                   | Nothing => pure Nothing
                                 run fuel (next x)
run Dry _                   = pure Nothing

%access public export

Service : (iface : reqType -> Type) -> Type -> Type
Service iface a = Process iface a Ready Looping

Client : Type -> Type
Client a = Process NoRecv a Ready Ready

partial export
runProc : Process iface () in_state out_state -> IO ()
runProc proc = run forever proc *> pure ()
