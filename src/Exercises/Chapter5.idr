module Exercises.Chapter5

import Data.Vect

%access  public export
%default partial


-- Exercise 5.3.5.1

||| Read input from the console until the user enters a blank line.
readToBlank : IO (List String)
readToBlank = go []
  where
    go : List String -> IO (List String)
    go acc = do
      "" <- getLine | input => do go (input :: acc)
      pure (reverse acc)


-- Exercise 5.3.5.2

total
printErrorAndClose : (err : FileError) -> (file : File) -> IO ()
printErrorAndClose err file = printLn err *> closeFile file

total
appendLine : (file : File) -> (line : String) -> IO ()
appendLine file line = do
  Right () <- fPutStrLn file line | Left err => printErrorAndClose err file
  putStr "."

||| Read input from the console until the user enters a blank line,
||| then read a file name from the console, and write the input to the file.
readAndSave : IO ()
readAndSave = do
  putStrLn "Input:"
  lines      <- readToBlank
  putStr "Filename: "
  filename   <- getLine
  Right file <- openFile filename WriteTruncate | Left err => printLn err
  -- for_ lines (fPutStrLn file)
  for_ {b = ()} lines $ \line => do
    Right () <- fPutStrLn file line | Left err => printErrorAndClose err file
    putStr "."
  putStrLn "done"


-- Exercise 5.3.5.3

total
empty : (Z ** Vect Z String)
empty = (_ ** [])

readVectFile' : (file : File)
              -> (lines : (n ** Vect n String))
              -> IO (m ** Vect m String)
readVectFile' file (n ** lines) = do
  False <- fEOF file | True => pure (n ** (reverse lines))
  Right line <- fGetLine file
    | Left  err  => do printErrorAndClose err file
                       pure empty
  readVectFile' file (_ ** (line :: lines))


||| Read the contents of a file into a dependent pair containing a length and a
||| `Vect` of that length. If there are any errors, return the empty vector.
readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
  Right file <- openFile filename Read
    | Left err => do printLn err
                     pure empty
  readVectFile' file empty
