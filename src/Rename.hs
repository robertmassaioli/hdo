module Rename 
   ( executeRenameCommand
   ) where

import Database.HDBC
import TodoArguments
import Configuration
import Util

import Data.Maybe (catMaybes)
import Control.Monad.Trans.Maybe

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
         fromIds <- fmap catMaybes $ mapM (runMaybeT . getListId conn) (fromListPath command)
         run conn (updateItems fromIds) [toSql toId]
         run conn (deleteOldLists fromIds) []
         commit conn
         disconnect conn
         putStr $ "Successfully renamed: "
         putStr . show $ fromIds
         putStr " => "
         print toId
   where
      updateItems :: [Integer] -> String
      updateItems fromItems = "UPDATE items SET list_id = ? WHERE " ++ createOrList "list_id =" fromItems

      deleteOldLists :: [Integer] -> String
      deleteOldLists fromItems = "DELETE FROM lists where " ++ createOrList "id =" fromItems
