||| An interactive data store.
module Main

import Control.Arrow
import Control.Category
import Data.Morphisms
import Data.Vect

%default total
%access  public export

-- 6.3.1

infixr 5 .+.

data Schema = ||| A schema containing a single character.
              SChar
            | ||| A schema containing a single string.
              SString
            | ||| A schema containing a single integer.
              SInt
            | ||| A schema combining two smaller schemas.
              (.+.) Schema Schema

||| Convert a schema to a concrete type.
SchemaType : Schema -> Type
SchemaType SChar     = Char
SchemaType SString   = String
SchemaType SInt      = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

||| A sized data store for strings, referable by numeric identifier.
record DataStore where
  constructor Data
  ||| The schema of the store.
  schema : Schema               -- 6.3.2
  ||| The number or stored items.
  size   : Nat                  -- N.B. This covers Exercise 4.3.5.1.
  ||| The contents of the store.
  items  : Vect size (SchemaType schema)

%name DataStore store

||| A REPL response, i.e. possibly a pair of reponse to print and data store.
REPLResponse : Type
REPLResponse = Maybe (String, DataStore)

||| Add an item to a data store.
||| @ store the store to add to.
||| @ item  the item to add.
addToStore : (store : DataStore)
           -> (item : SchemaType (schema store))
           -> DataStore
addToStore (Data schema _ store) newitem = Data schema _ (addToData store)
  where
    addToData : Vect k (SchemaType schema) -> Vect (S k) (SchemaType schema)
    addToData []              = [newitem]
    addToData (item :: items) = item :: addToData items

||| An data store command.
data Command : Schema -> Type where
   ||| Set the schema.
   ||| @ newschema the schema to set.
   SetSchema : (newschema : Schema)            -> Command schema
   ||| Add an item.
   ||| @ item an item to add.
   Add       : (item      : SchemaType schema) -> Command schema
   ||| Get an item by ID.
   ||| @ id an ID to look up.
   Get       : (id        : Integer)           -> Command schema
   ||| Search the data store for entries containing a substring.
   ||| @ query a substring to match.
   Search    : (query     : String)            -> Command schema
   ||| Quit the REPL.
   Quit      : Command schema

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SChar input with (unpack input)
  | ('\'' :: xs) with (span (/= '\'') xs)
    | ([char], '\'' :: rest) = Just (char, ltrim (pack rest))
    | _ = Nothing
  | _ = Nothing
parsePrefix SString input with (unpack input)
  | ('"' :: xs) with (span (/= '"') xs)
    | (quoted, '"' :: rest) = Just (pack quoted, ltrim (pack rest))
    | _                     = Nothing
  | _ = Nothing
parsePrefix SInt    input with (span isDigit input)
  | ("",  rest) = Nothing
  | (num, rest) = Just (cast num, ltrim rest)
parsePrefix (schemaL .+. schemaR) input = do
  (left,  input')  <- parsePrefix schemaL input
  (right, input'') <- parsePrefix schemaR input'
  pure ((left, right), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input with (parsePrefix schema input)
  | Just (res, "") = Just res
  | Just _         = Nothing
  | Nothing        = Nothing

parseSchema : List String -> Maybe Schema
parseSchema ("Char"   :: []) = Just  SChar
parseSchema ("Char"   :: xs) = Just (SChar .+.) <*> parseSchema xs
parseSchema ("String" :: []) = Just  SString
parseSchema ("String" :: xs) = Just (SString .+.) <*> parseSchema xs
parseSchema ("Int"    :: []) = Just  SInt
parseSchema ("Int"    :: xs) = Just (SInt    .+.) <*> parseSchema xs
parseSchema _ = Nothing

||| Parse a command from the interactive prompt.
||| @ cmd  a string to attempt to parse as a command.
||| @ args arguments to pass to a parsed command.
parseCommand : (schema : Schema)
             -> (cmd, args : String)
             -> Maybe (Command schema)
parseCommand schema "add"    str   = pure Add <*> parseBySchema schema str
parseCommand schema "get"    val   = if   all isDigit (unpack val)
                                     then Just $ Get (cast val)
                                     else Nothing
parseCommand schema "search" ""    = Nothing
parseCommand schema "search" query = Just (Search query)
parseCommand schema "quit"   _     = Just Quit
parseCommand schema "schema" rest  = pure SetSchema <*> parseSchema (words rest)
parseCommand schema _        _     = Nothing

||| Attempt to parse a string as a command.
||| @ schema the schema of a data store.
||| @ input  a string to attempt to parse.
parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input with (span (/= ' ') input)
  | (cmd, args) = parseCommand schema cmd (ltrim args)

display : SchemaType schema -> String
display {schema = SChar}      item  = show item
display {schema = SString}    item  = show item
display {schema = SInt}       item  = show item
display {schema = (x .+. y)} (a, b) = display a ++ ", " ++ display b

||| Add an item to a data store and return a REPL response.
||| @ item  an item to add.
||| @ store a store to add to.
addItem :  (store : DataStore)
        -> (item : SchemaType (schema store))
        -> REPLResponse
addItem store item =
  Just ("ID " ++ show (size store) ++ "\n", addToStore store item)

||| Get an entry from a data store by ID and return a REPL response.
||| @ id    an ID to look up.
||| @ store a store possibly containing a matching entry.
getEntry : (id : Integer) -> (store : DataStore) -> REPLResponse
getEntry id store with (integerToFin id (size store))
  | Nothing = Just ("Out of range\n", store)
  | Just i  = Just (display (index i (items store)) ++ "\n", store)


-- Exercise 6.3.8.2

||| Pretty print all entries in a data store and return a REPL response.
||| @ store a data store whose entries to pretty print
getAllEntries : (store : DataStore) -> REPLResponse
getAllEntries store@(Data _ Z []) = Just ("No entries\n", store)
getAllEntries store              = go [] (items store)
  where
    go :  (acc : Vect n String)
       -> (items : Vect m (SchemaType (schema store)))
       -> REPLResponse
    go     acc []      = Just (unlines (toList (reverse acc)) ++ "\n", store)
    go {n} acc (x::xs) = go ((show n ++ " => " ++ display x) :: acc) xs

doSearch :  (query : String)
         -> (store : Vect n (SchemaType schema))
         -> List (Nat, String)
doSearch query [] = []
doSearch query (item :: items) =
  let matches = pure go <*> doSearch query items
      itemStr = display item in
      if query `isInfixOf` toLower itemStr then
        (0,itemStr) :: matches
      else
        matches
      where
        go = applyMor $ first (arrow S)

search : (query : String) -> (store : DataStore) -> REPLResponse
search query store with (doSearch query (items store))
  | []      = Just ("Not found\n", store)
  | matches = Just (unwords (map format matches) ++ "\n", store)
      where format (k,x) = "(" ++ show k ++ ", " ++ x ++ ")"

setSchema : (store : DataStore) -> (schema : Schema) -> Maybe DataStore
setSchema (Data _ Z items) schema = Just (Data schema _ [])
setSchema _                _      = Nothing

||| Process user input from the REPL.
||| @ store a data store
||| @ input a user-entered string, possibly representing a valid command.
processInput : (store : DataStore) -> (input : String) -> REPLResponse
-- FIXME: This is smelly, but gets the job done.
processInput store "get"     = getAllEntries store
processInput store input with (parse (schema store) input)
  | Nothing                  = Just ("Invalid command\n", store)
  | Just (Add       item)    = addItem  store item
  | Just (Get       id)      = getEntry id    store
  | Just (Search    query)   = search   (toLower query) store
  | Just (SetSchema schema') =
      (pure (MkPair "OK\n") <*> setSchema store schema') <|>
       pure ("Can't update schema\n", store)
  | Just Quit                = Nothing

||| Run a REPL, processing commands and maintaing a data store.
partial
main : IO ()
main = replWith (Data (SString .+. SString .+. SInt) _ []) "> " processInput
