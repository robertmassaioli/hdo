module Move (
   executeMoveCommand
   ) where

import TodoArguments
import Configuration

executeMoveCommand :: Config -> TodoCommand -> IO ()
executeMoveCommand config command@(Move {}) = do
   putStrLn "Not implemented yet."
