||| An interactive data store.
module Main

import Data.Vect

%default total
%access  public export

data DataStore : Type where
  Data : (size : Nat) -> (items : Vect size String) -> DataStore

%name DataStore store

size : (store : DataStore) -> Nat
size (Data size' _) = size'

items : (store : DataStore) -> Vect (size store) String
items (Data _ items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (Data _ items) item = Data _ (item :: items)

data Command = Add String
             | Get Integer
             | Quit

parseCommand : (cmd, args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val with (all isDigit (unpack val))
  | False = Nothing
  | True  = Just (Get (cast val))
parseCommand "quit" "" = Just Quit
parseCommand _      _  = Nothing

parse : (input : String) -> Maybe Command
parse input with (span (/= ' ') input)
  | (cmd, args) = parseCommand cmd (ltrim args)

addItem : (item : String) -> (store : DataStore) -> Maybe (String, DataStore)
addItem item store =
  Just ("ID " ++ show (size store) ++ "\n", addToStore store item)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store with (integerToFin pos (size store))
  | Nothing = Just ("Out of range\n", store)
  | Just id = Just (index id (items store) ++ "\n", store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input with (parse input)
  | Nothing         = Just ("Invalid command\n", store)
  | Just (Add item) = addItem  item store
  | Just (Get pos)  = getEntry pos  store
  | Just Quit       = Nothing

partial
main : IO ()
main = replWith (Data _ []) "> " processInput
