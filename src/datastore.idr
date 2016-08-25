||| An interactive data store.
module Main

import Control.Arrow
import Control.Category
import Data.Morphisms
import Data.Vect

%default total

------------------------------------------- [ Exercise 6.3.1 // Revised 10.3.2 ]

infixr 5 .+.

public export
data Schema = ||| A schema containing a single character.
              SChar
            | ||| A schema containing a single string.
              SString
            | ||| A schema containing a single integer.
              SInt
            | ||| A schema combining two smaller schemas.
              (.+.) Schema Schema

||| Convert a schema to a concrete type.
public export
SchemaType : Schema -> Type
SchemaType SChar     = Char
SchemaType SString   = String
SchemaType SInt      = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

||| A sized data store for strings, referable by numeric identifier.
export
record DataStore (schema : Schema) where
  constructor MkData
  ||| The number or stored items.
  size   : Nat                  -- N.B. This covers Exercise 4.3.5.1.
  ||| The contents of the store.
  items  : Vect size (SchemaType schema)

%name DataStore store

export
empty : DataStore schema
empty = MkData 0 []

||| Add an item to a data store.
||| @ item  the item to add.
||| @ store the store to add to.
export
addToStore : (item  : SchemaType schema)
          -> (store : DataStore schema)
          -> DataStore schema
addToStore item (MkData _ items) = MkData _ (item :: items)

--------------------------------------------------------------------- [ 10.3.3 ]

public export
data StoreView : DataStore schema -> Type where
          SNil : StoreView empty
          SAdd : (rec : StoreView store) -> StoreView (addToStore value store)

storeViewHelp : (items : Vect size (SchemaType schema))
             -> StoreView (MkData size items)
storeViewHelp []          = SNil
storeViewHelp (val :: xs) = SAdd (storeViewHelp xs)

export
storeView : (store : DataStore schema) -> StoreView store
storeView (MkData size items) = storeViewHelp items

export
filterKeys : (test  : SchemaType schema -> Bool)
          -> (store : DataStore (SString .+. schema))
          -> List String
filterKeys test store with (storeView store)
  filterKeys test                          store  | SNil     = []
  filterKeys test (addToStore (key, value) store) | SAdd rec =
    if test value
       then key :: filterKeys test store | rec
       else        filterKeys test store | rec

--------------------------------------------------------------------------------

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
parseCommand schema "get"    val   = if all isDigit (unpack val)
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
addItem : (item : SchemaType schema)
       -> (store : DataStore schema)
       -> Maybe (String, DataStore schema)
addItem item store =
  Just ("ID " ++ show (size store) ++ "\n", addToStore item store)

||| Get an entry from a data store by ID and return a REPL response.
||| @ id    an ID to look up.
||| @ store a store possibly containing a matching entry.
getEntry : (id : Integer) -> (store : DataStore schema) -> Maybe (String, DataStore schema)
getEntry id store with (integerToFin id (size store))
  | Nothing = Just ("Out of range\n", store)
  | Just i  = Just (display (index i (items store)) ++ "\n", store)

----------------------------------------------------------- [ Exercise 6.3.8.2 ]

showItems : DataStore schema -> List (SchemaType schema)
showItems input with (storeView input)
  showItems empty                   | SNil       = []
  showItems (addToStore item store) | (SAdd rec) = item :: showItems store | rec

||| Pretty print all entries in a data store and return a REPL response.
||| @ store a data store whose entries to pretty print
getAllEntries : (store : DataStore schema) -> Maybe (String, DataStore schema)
getAllEntries store@(MkData Z []) = Just ("No entries\n", store)
getAllEntries store               = Just (unlines (reverse (go 0 (showItems store))), store)
  where
    go : Nat -> List (SchemaType schema) -> List String
    go _ []      = []
    go n (x::xs) = (show n ++ " => " ++ display x) :: (go (S n) xs)

doSearch : (query : String)
        -> (store : Vect n (SchemaType schema))
        -> List (Nat, String)
doSearch query [] = []
doSearch query (item :: items) =
  let matches = pure go <*> doSearch query items
      itemStr = display item in
      if query `isInfixOf` toLower itemStr
         then (0,itemStr) :: matches
         else matches
  where
    go = applyMor $ first (arrow S)

search : (query : String) -> (store : DataStore schema) -> Maybe (String, DataStore schema)
search query store with (doSearch query (items store))
  | []      = Just ("Not found\n", store)
  | matches = Just (unwords (map format matches) ++ "\n", store)
      where format (k,x) = "(" ++ show k ++ ", " ++ x ++ ")"

setSchema : (schema' : Schema) -> (store : DataStore schema) -> Maybe (DataStore schema')
setSchema schema' (MkData Z items) = Just (MkData _ [])
setSchema _       _                = Nothing

||| Process user input from the REPL.
||| @ store a data store
||| @ input a user-entered string, possibly representing a valid command.
partial
processInput : (store : DataStore schema) -> (input : String) -> Maybe (String, DataStore schema)
-- FIXME: This is smelly, but gets the job done.
processInput store "get"     = getAllEntries store
processInput {schema} store input with (parse schema input)
  | Nothing                  = Just ("Invalid command\n", store)
  | Just (Add       item)    = addItem  item  store
  | Just (Get       id)      = getEntry id    store
  | Just (Search    query)   = search   (toLower query) store
  -- | Just (SetSchema schema') =
  --     (pure (MkPair "OK\n") <*> setSchema schema' store) <|>
  --      pure ("Can't update schema\n", store)
  | Just Quit                = Nothing

||| Run a REPL, processing commands and maintaing a data store.
partial
main : IO ()
main = replWith (the (DataStore (SString .+. SString .+. SInt)) empty) "> " processInput
