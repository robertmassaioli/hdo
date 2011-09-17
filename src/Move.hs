module Move (
   executeMoveCommand
   ) where

import Database.HDBC
import Data.List (intersperse)
import Text.Parsec
import Control.Monad.Trans.Maybe

import TodoArguments
import Configuration
import Filter
import Range
import Util

{- 
 - The purpose of the move command is to move items between lists allowing a kind
 - of cherry picking feature to emerge.
 -}
executeMoveCommand :: Config -> TodoCommand -> IO ()
executeMoveCommand config command@(Move {}) = do
   mconn <- getDatabaseConnection config command
   case parseMoveRanges . itemRanges $ command of
      Left _ -> putStrLn "The range string was formatted incorrectly."
      Right ranges -> do 
         case mconn of
            Nothing -> gracefulExit
            Just conn -> do
               -- Get the list id of the first list
               toId <- getOrCreateListId conn . Just . toListPath $ command
               -- Get all of the elements from the database and move them to that list
               run conn (itemUpdateString ranges) [toSql toId]
               commit conn
               disconnect conn
               putStrLn $ "Moved " ++ (concat . intersperse "," . itemRanges $ command) ++ " => " ++ toListPath command
   where
      itemUpdateString :: [Range Integer] -> String
      itemUpdateString ranges = "UPDATE items SET list_id = ? WHERE " ++ rangeToSqlOr "" (mergeRanges ranges)

      parseMoveRanges :: [String] -> Either ParseError [Range Integer]
      parseMoveRanges [] = Right []
      parseMoveRanges xs = parse (parseRanges ',') "(move command)" . concat . intersperse "," $ xs
      
