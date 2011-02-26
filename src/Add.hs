module Add 
   ( executeAddCommand
   ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad (unless)
import Control.Monad.Reader
import Control.Monad.Maybe
import System.Console.Haskeline
import System.IO (hFlush, stdout)

import TodoArguments
import Util
import Configuration
import DataTypes

-- TODO This function needs to be a MaybeT IO ()
executeAddCommand :: Config -> TodoCommand -> IO ()
executeAddCommand config addFlags = do
   prettyShow addFlags
   mconn <- getDatabaseConnection config addFlags
   case mconn of
      Nothing -> gracefulExit
      Just conn -> do
         d <- getData addFlags
         case d of
            Nothing -> putStrLn "Need a comment and priority to add a new item, or reacting to early termination."
            Just (comment, pri, tags) -> do
               listId <- getOrCreateListId conn (listPath addFlags)
               putStrLn $ "List id: " ++ show listId
               run conn addInsertion [toSql listId, toSql comment, toSql $ fromEnum StateNotDone, toSql pri]
               itemId <- getLastId conn
               run conn "INSERT INTO item_events (item_id, item_event_type, occurred_at) VALUES (?, ?, datetime())" [toSql itemId, toSql $ fromEnum EventAdd]
               unless (null tags) $ do
                  tagIds <- findOrCreateTags conn itemId tags
                  insertStatement <- prepare conn "INSERT INTO tag_map (item_id, tag_id, created_at) VALUES (?,?, datetime())"
                  mapM_ (execute insertStatement) [[toSql itemId, tag] | tag <- map toSql tagIds]
               commit conn
               disconnect conn
               putStrLn $ "Added item " ++ show itemId ++ " successfully."
   where 

      getData :: TodoCommand -> IO (Maybe (String, String, [String]))
      getData addCommand = runInputT defaultSettings (runMaybeT getDataHelper)
         where
            getDataHelper :: MaybeT (InputT IO) (String, String, [String])
            getDataHelper = do
               Just description <- lift $ getInputLine "comment> "
               guard (not $ null description)
               Just pri <- lift $ case priority addCommand of
                  Nothing -> getInputLine "priority> "
                  Just pri -> getInputLineWithInitial "priority> " (show pri, "")
               guard (not $ null pri)
               Just tags <- lift $ getInputLine "tags> "
               return (description, pri, words tags)

      putStrFlush :: String -> IO ()
      putStrFlush s = putStr s >> hFlush stdout 

      addInsertion :: String
      addInsertion = "INSERT INTO items (list_id, description, current_state, created_at, priority)" ++ 
                     "VALUES (?, ?, ?, datetime(), ?)"

      -- TODO this function only handles the top level list id but there may be more to it, more
      -- levels deep
      getOrCreateListId :: (IConnection c) => c -> Maybe String -> IO Integer
      getOrCreateListId _     Nothing     = return 1 
      getOrCreateListId _     (Just [])   = return 1
      getOrCreateListId conn  (Just xs)   = go Nothing (maybe [] id $ separateBy '/' xs)
         where 
            go :: Maybe Integer -> [String] -> IO Integer
            go Nothing []        = return 1
            go (Just listId) []  = return listId
            go listId (name:xs)  = do
               result <- fmap extractInteger $ quickQuery' conn "SELECT id FROM lists WHERE name = ? AND parent_id = ?" [toSql name, toSql listId]
               case result of 
                  Nothing -> do
                     run conn "INSERT INTO lists(name, hidden, created_at, parent_id) VALUES (?, 0, datetime(), ?)" [toSql name, toSql listId]
                     lastId <- getLastId conn
                     go (Just lastId) xs
                  existing -> do
                     go existing xs
