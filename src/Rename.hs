module Rename 
   ( executeRenameCommand
   ) where

import Database.HDBC
import TodoArguments
import Configuration
import Util

import Data.Maybe (catMaybes)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad (unless)

{- 
 - The entire purpose of the rename command is to move one or more lists into another list.
 - If the list that you are moving the elements to does not exist then you should create it.
 - Once you do then you should move every element from the list it currently belongs to to the
 - other list that you are supposed to be moving to.
 -}
executeRenameCommand :: Config -> TodoCommand -> IO ()
executeRenameCommand config command@(Rename {}) =
   if null . fromListPath $ command
      then putStrLn "No FROM_LISTS given but atleast one is required: doing nothing."
      else do
         mconn <- getDatabaseConnection config command
         case mconn of
            Nothing -> gracefulExit
            Just conn -> do
               toId <- getOrCreateListId conn (Just $ toListPath command)
               fromIds <- fmap catMaybes $ mapM (runMaybeT . getListId conn) (fromListPath command)
               if null fromIds
                  then putStrLn "Could not find any lists to move to the new one."
                  else do
                     run conn (updateItems fromIds) [toSql toId]
                     cleanOldLists conn fromIds
                     putStr "Successfully renamed: "
                     putStr . show $ fromIds
                     putStr " => "
                     print toId
               commit conn
               disconnect conn
   where
      updateItems :: [Integer] -> String
      updateItems fromItems = "UPDATE items SET list_id = ? WHERE " ++ createOrList "list_id =" fromItems

      cleanOldLists :: IConnection c => c -> [Integer] -> IO ()
      cleanOldLists _ [] = return ()
      cleanOldLists conn (oldList:xs) = do
         mcount <- fmap extractInteger $ quickQuery' conn "SELECT count(*) FROM items WHERE list_id = ?" [toSql oldList]
         case mcount of
            Nothing -> do
               putStrLn "Major error, somehow the SQL count did not return a number..."
               cleanOldLists conn xs
            Just count -> 
               if count == 0
                  then do
                     parent <- quickQuery' conn "SELECT parent_id FROM lists WHERE id = ?" [toSql oldList]
                     run conn "DELETE FROM lists where id = ?" [toSql oldList]
                     case parent of
                        [] -> cleanOldLists conn xs
                        [x] -> cleanOldLists conn $ maybe xs (\x -> xs ++ [x]) (extractInteger [x])
                  else cleanOldLists conn xs
