module Rename 
   ( executeRenameCommand
   ) where

import TodoArguments
import Configuration

executeRenameCommand :: Config -> TodoCommand -> IO ()
executeRenameCommand config command@(Rename {}) = do
   putStrLn "Not implemented yet"
