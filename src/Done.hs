module Done 
   ( executeDoneCommand
   ) where

import Control.Monad.Reader (lift)
import Data.List(sortBy, (\\), nub, intercalate)
import Database.HDBC
import Text.Parsec
import System.Console.Haskeline

import DataTypes
import Util
import TodoArguments
import Configuration
import Range
import Filter

executeDoneCommand :: Config -> TodoCommand -> IO ()
executeDoneCommand config doneCommand = do
   -- Todo replace this with withConnection
   mconn <- getDatabaseConnection config doneCommand
   case mconn of
      Nothing -> gracefulExit
      Just conn -> do
         getExistingElements conn mergedDoneRanges >>= mapM_ (markElementAsDone conn)
         commit conn
         disconnect conn
   where
      printDone :: [Integer] -> IO [Integer]
      printDone xs = do 
         putStrLn $ "Marking these id's as done: " ++ show xs
         return xs

      mergedDoneRanges = getDoneRanges . doneRanges $ doneCommand

      getDoneRanges :: String -> [Range Integer]
      getDoneRanges input = case parse (parseRanges ',') "(done_ranges)" input of
                              Left _ -> []
                              Right x -> mergeRanges x

      getExistingElements :: (IConnection c) => c -> [Range Integer] -> IO [Integer]
      getExistingElements conn mdr = 
         case mdr of
            [] -> return []
            mdrxs -> do 
                  existing <- fmap getListOfId $ quickQuery conn existingItems []
                  done <- fmap getListOfId $ quickQuery conn alreadyDone [toSql $ fromEnum StateDone]
                  return (existing \\ done)
               where 
                  getListOfId :: [[SqlValue]] -> [Integer]
                  getListOfId = map $ fromSql . head

                  existingItems = "SELECT i.id from items i WHERE " ++ sqlRange
                  alreadyDone = "SELECT i.id from items i WHERE i.current_state >= ? AND (" ++ sqlRange ++ ")"

                  sqlRange = rangeToSqlOr "i" mdrxs


      markElementAsDone :: (IConnection c) => c -> Integer -> IO ()
      markElementAsDone conn itemId = do 
         [[sqlDes]] <- quickQuery' conn "SELECT description FROM items WHERE id = ?" [toSql itemId]
         runInputT defaultSettings $ markDoneHelper (fromSql sqlDes)
         where
            markDoneHelper :: String -> InputT IO ()
            markDoneHelper description = do
               lift . putStrLn $ show itemId ++ ": " ++ description
               comment <- getInputLine "comment> "
               lift $ run conn "INSERT INTO item_events (item_id, item_event_type, event_description, occurred_at) VALUES (?, ?, ?, datetime())" [toSql itemId, toSql $ fromEnum EventDone, toSql comment]
               lift $ run conn "UPDATE items SET current_state = ? WHERE id = ?" [toSql $ fromEnum StateDone, toSql itemId]
               return ()
