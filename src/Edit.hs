module Edit 
   ( executeEditCommand 
   ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Maybe
import Database.HDBC
import Data.Char (isDigit)
import Data.List
import Text.Parsec
import System.Console.Haskeline

import TodoArguments
import Util
import Configuration
import Filter
import Range
import DataTypes

executeEditCommand :: Config -> TodoCommand -> IO ()
executeEditCommand config editCommand = do
   mconn <- getDatabaseConnection config editCommand
   case mconn of
      Nothing -> gracefulExit
      Just conn -> do
         sequence_ . intersperse putNewline . map (editSingleId conn) . getEditRanges . editRanges $ editCommand
         commit conn
         disconnect conn
      where
         getEditRanges :: String -> [Integer]
         getEditRanges input = case parse (parseRanges ',') "(edit_ranges)" input of
                                 Left _ -> []
                                 Right x -> fromMergedRanges x

         editSingleId :: (IConnection c) => c -> Integer -> IO ()
         editSingleId conn id = do
            putStrLn $ "Now editing item: " ++ show id
            d <- runMaybeT $ getEditData conn id
            case d of
               Nothing -> putStrLn $ "Could not find data for id: " ++ show id
               Just oldData@(oldDesc,_,oldTags) -> do 
                  newData <- runInputT defaultSettings (runMaybeT (askEditQuestions oldData))
                  case newData of
                     Nothing -> putStrLn "Invalid input or early termination."
                     Just (desc, pri, tags) -> do 
                        -- TODO create an edit event here to log the change
                        run conn updateItem [toSql desc, toSql pri, toSql id]
                        run conn "INSERT INTO item_events (item_id, item_event_type, event_description, occurred_at) VALUES (?,?,?, datetime())" [toSql id, toSql $ fromEnum EventEdit, toSql oldDesc]
                        cs <- prepare conn createStatement
                        ds <- prepare conn deleteStatement
                        findOrCreateTags conn id (tags \\ oldTags) >>= mapM_ (createTagMapping cs id)
                        getTagMapIds conn id (oldTags \\ tags) >>= mapM_ (deleteTagMapping ds id)
                        putStrLn $ "Successfully updated item " ++ show id ++ "."
                        -- please note that we intentionally do not delete tags here; just the
                        -- mappings, we leave them around for later use. The 'htodo clean' or maybe
                        -- 'htodo gc' command will do that cleanup I think.
            where
               updateItem = "UPDATE items SET description = ?, priority = ? where id = ?"

               getTagMapIds :: (IConnection c) => c -> Integer -> [String] -> IO [Integer]
               getTagMapIds _ _ [] = return []
               getTagMapIds conn itemId tags = fmap (map fromSql . concat) $ quickQuery' conn theQuery [toSql itemId]
                  where
                     theQuery = "SELECT tm.tag_id from tag_map tm, tags t where t.id = tm.tag_id and (" ++ tagOrList ++ ") and tm.item_id = ?"
                     tagOrList = createOrList "t.tag_name =" tags

               deleteTagMapping :: Statement -> Integer -> Integer -> IO ()
               deleteTagMapping statement itemId tagId = execute statement [toSql itemId, toSql tagId] >> return ()

               createTagMapping :: Statement -> Integer -> Integer -> IO ()
               createTagMapping statement itemId tagId = execute statement [toSql itemId, toSql tagId] >> return ()

               createStatement = "INSERT INTO tag_map (item_id, tag_id, created_at) VALUES (?,?, datetime())"
               deleteStatement = "DELETE FROM tag_map WHERE item_id = ? AND tag_id = ?"

               getDescAndPri :: [[SqlValue]] -> Maybe (String, Integer)
               getDescAndPri vals = case vals of
                  [[a,b]]  -> Just (fromSql a, fromSql b)
                  _        -> Nothing

               getEditData :: (IConnection c) => c -> Integer -> MaybeT IO (String, Integer, [String])
               getEditData conn id = do
                  [[sqlDescription, sqlPriority]] <- lift $ quickQuery' conn "select description, priority from items where id = ?" [toSql id]
                  sqlTags <- lift $ quickQuery' conn "select t.tag_name from tags t, tag_map tm, items i where i.id = tm.item_id and tm.tag_id = t.id and i.id = ?" [toSql id]
                  return (fromSql sqlDescription, fromSql sqlPriority, map fromSql (concat sqlTags))

               askEditQuestions :: (String, Integer, [String]) -> MaybeT (InputT IO) (String, Integer, [String])
               askEditQuestions (desc, pri, tags) = do
                  Just newDesc <- lift $ getInputLineWithInitial "comment> " $ defInit desc
                  guard (not $ null newDesc)
                  Just newPri <- lift $ getInputLineWithInitial "priority> " . defInit $ show pri 
                  guard (not $ null newPri)
                  guard (all isDigit newPri) -- the priority must be a digit
                  Just newTags <- lift $ getInputLineWithInitial "tags> " . defInit $ unwords tags
                  return (newDesc, read newPri, nub . words $ newTags)
                  where defInit a = (a, "")
