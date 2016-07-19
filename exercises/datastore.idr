||| An interactive data store.
module Main

import Data.Vect

%default total
%access  public export

||| A sized data store for strings, referable by numeric identifier.
record DataStore where
  constructor Data
  ||| The number or stored items.
  size  : Nat                   -- N.B. This covers Exercise 4.3.5.1.
  ||| The contents of the store.
  items : Vect size String

%name DataStore store

||| A REPL response, i.e. possibly a pair of reponse to print and data store.
REPLResponse : Type
REPLResponse = Maybe (String, DataStore)

||| Add an item to a data store.
||| @ store the store to add to.
||| @ item  the item to add.
addToStore : (store : DataStore) -> (item : String) -> DataStore
addToStore (Data _ items) item = Data _ (items ++ [item])

||| An data store command.
data Command = ||| Add an item.
               ||| @ item an item to add.
               Add    String
             | ||| Get an item by ID.
               ||| @ id an ID to look up.
               Get    Integer
             | ||| Search the data store for entries containing a substring.
               ||| @ query a substring to match.
               Search String
             | ||| Quit the REPL.
               Quit

||| Parse a command from the interactive prompt.
||| @ cmd  a string to attempt to parse as a command.
||| @ args arguments to pass to a parsed command.
parseCommand : (cmd, args : String) -> Maybe Command
parseCommand "add"    str   = Just (Add str)
parseCommand "get"    val   = if   all isDigit (unpack val)
                              then Just $ Get (cast val)
                              else Nothing
parseCommand "search" query = Just (Search query)
parseCommand "quit"   ""    = Just Quit
parseCommand _        _     = Nothing

||| Attempt to parse a string as a command.
||| @ input a string to attempt to parse.
parse : (input : String) -> Maybe Command
parse input with (span (/= ' ') input)
  | (cmd, args) = parseCommand cmd (ltrim args)

||| Add an item to a data store and return a REPL response.
||| @ item  an item to add.
||| @ store a store to add to.
addItem : (item : String) -> (store : DataStore) -> REPLResponse
addItem item store =
  Just ("ID " ++ show (size store) ++ "\n", addToStore store item)

||| Get an entry from a data store by ID and return a REPL response.
||| @ id    an ID to look up.
||| @ store a store possibly containing a matching entry.
getEntry : (id : Integer) -> (store : DataStore) -> REPLResponse
getEntry id store with (integerToFin id (size store))
  | Nothing = Just ("Out of range\n", store)
  | Just i  = Just (index i (items store) ++ "\n", store)

doSearch : (query : String)
         -> (item : String) -> (matches : List String)
         -> List String
doSearch query item = if query `isInfixOf` toLower item then (::) item else id

search : (query : String) -> (store : DataStore) -> REPLResponse
search query store with (foldr (doSearch (toLower query)) [] (items store))
  | []      = Just ("Not found\n", store)
  | matches = Just (unwords matches ++ "\n", store)

||| Process user input from the REPL.
||| @ store a data store
||| @ input a user-entered string, possibly representing a valid command.
processInput : (store : DataStore) -> (input : String) -> REPLResponse
processInput store input with (parse input)
  | Nothing             = Just ("Invalid command\n", store)
  | Just (Add    item)  = addItem  item store
  | Just (Get    id)    = getEntry id   store
  | Just (Search query) = search   query store
  | Just Quit           = Nothing

||| Run a REPL, processing commands and maintaing a data store.
partial
main : IO ()
main = replWith (Data _ []) "> " processInput
