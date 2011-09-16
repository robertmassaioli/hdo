module Rename 
   ( executeRenameCommand
   ) where

import TodoArguments
import Configuration
import Util

import Data.Maybe (catMaybes)

{- 
 - The entire purpose of the rename command is to move one or more lists into another list.
 - If the list that you are moving the elements to does not exist then you should create it.
 - Once you do then you should move every element from the list it currently belongs to to the
 - other list that you are supposed to be moving to.
 -}
executeRenameCommand :: Config -> TodoCommand -> IO ()
executeRenameCommand config command@(Rename {}) = do
   print command
   mconn <- getDatabaseConnection config command
   case mconn of
      Nothing -> gracefulExit
      Just conn -> do
         toId <- getOrCreateListId conn (Just $ toListPath command)
         fromIds <- fmap catMaybes $ mapM (getListId conn) (fromListPath command)
         putStr . show $ fromIds
         putStr " => "
         putStrLn . show $ toId
