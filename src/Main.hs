import Control.Monad (unless)
import Data.Maybe (catMaybes)
import Data.List (intersperse)

import Filter
import TodoArguments

main = do
   command <- getCommandInput
   print command
   executeCommand command

executeCommand :: TodoCommand -> IO ()
executeCommand x@(Show {}) = executeShowCommand x
executeCommand x@(Init {}) = executeShowCommand x
executeCommand x@(Add {}) = executeShowCommand x
executeCommand x@(Edit {}) = executeShowCommand x
executeCommand x@(Done {}) = executeShowCommand x

executeShowCommand :: TodoCommand -> IO ()
executeShowCommand (Show sa sd f_one f_two) = do
   putStrLn "Should be showing stuff now."
   print filter_str
   unless (filter_str == "") $ print (getFilters filter_str)
   where
      filter_str = concat . intersperse "," . catMaybes $ [f_one, f_two]
