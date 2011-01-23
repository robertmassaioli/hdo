import Control.Monad (unless, liftM)
import Control.Monad.Reader

import Control.Monad.Maybe
import Data.Maybe (catMaybes)

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Time (LocalTime)
import Data.List(sortBy, intersperse)
import Data.Ord(comparing)
import Data.Maybe(fromMaybe)

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token

import Text.Show.Pretty

import System.Directory(doesDirectoryExist, createDirectory)
import System.FilePath( (</>) )
import System.IO(hFlush, stdout)

import System.Console.Haskeline

import Filter
import TodoArguments
import Configuration

-- Use Haskeline for tde!!!

prettyShow :: (Show a) => a -> IO ()
prettyShow = putStrLn . ppShow

main = do
   config <- defaultConfig
   runReaderT setupAppDir config
   prettyShow config
   command <- getCommandInput
   prettyShow command
   executeCommand config command

setupAppDir :: ReaderT Config IO ()
setupAppDir = do
   config <- ask
   liftIO $ do
      appDirExists <- doesDirectoryExist (defaultAppDirectory config)
      unless appDirExists $ do
         createDirectory (defaultAppDirectory config)
         createDatabase 
            (defaultAppDirectory config </> defaultDatabaseName config) 
            (defaultSchemaDir config </> create_file)
   where
      create_file :: FilePath
      create_file = "create_database.sqlite3.read"

createDatabase :: FilePath -> FilePath -> IO ()
createDatabase databaseFile schemaFile = do
   conn <- connectSqlite3 databaseFile
   putStrLn databaseFile
   createCommands <- readFile schemaFile
   withTransaction conn (createEverything createCommands)
   disconnect conn
   where
      createEverything :: (IConnection conn) => String -> conn -> IO ()
      createEverything createCommands conn = mapM_ (quickQuery'' conn) . lines $ createCommands
         where quickQuery'' conn qry = quickQuery' conn qry []

executeCommand :: Config -> TodoCommand -> IO ()
executeCommand c x@(Show {}) = executeShowCommand c x
executeCommand c x@(Init {}) = executeInitCommand c x
executeCommand c x@(Add {}) = executeAddCommand c x
executeCommand c x@(Edit {}) = executeEditCommand c x
executeCommand c x@(Done {}) = executeDoneCommand c x

executeShowCommand :: Config -> TodoCommand -> IO ()
executeShowCommand config showFlags = do
   conn <- getDatabaseConnection
   unless (filter_str == "") $ print (getFilters filter_str)
   case showUsingTags showFlags of
      Nothing -> do 
         getTodoItems conn (generateQuery []) >>= displayItems
      Just x -> case separateCommas x of
                  Nothing -> putStrLn "Invalid text was placed in the tags."
                  Just x -> getTodoItems conn (generateQuery x) >>= displayItems
   disconnect conn
   where
      filter_str = concat . intersperse "," . catMaybes $ [showUsingFilter showFlags, showFilterExtra showFlags]

      generateQuery :: [String] -> String
      generateQuery [] = "select i.* from items i where 1 = 1"
      generateQuery xs = queryLeft ++ " AND (" 
                         ++ (concat . intersperse " OR " . map (\s -> "t.tag_name = \"" ++ s ++ "\"") $ xs)
                         ++ ")"

      queryLeft = "SELECT i.* from items i, tags t, tag_map tm where "
                   ++ "i.id = tm.item_id AND "
                   ++ "tm.tag_id = t.id"

displayItems :: [Item] -> IO ()
displayItems = mapM_ (displayItemHelper 0)
   where
      displayItemHelper :: Int -> Item -> IO ()
      displayItemHelper level item = do
         putStr $ replicate (level * 3 + 1) ' '
         putStr $ show (itemId item) ++ ". "
         putStrLn $ itemDescription item
         mapM_ (displayItemHelper (level + 1)) (itemChildren item)
   
getTodoItems :: (IConnection c) => c -> String -> IO [Item]
getTodoItems conn baseQuery = do
   topLevels <- quickQuery' conn topLevelQuery [SqlNull]
   fmap sortItems $ mapM createChild topLevels
   where
      topLevelQuery = baseQuery ++ " AND i.parent_id is ?"
      childQuery = baseQuery ++ " AND i.parent_id = ?"

      createChild :: [SqlValue] -> IO Item
      createChild [iid, ide, ica, _, ipr] = do
         let this_id = fromSql iid :: Integer
         children <- mapM createChild =<< quickQuery' conn childQuery [toSql this_id]
         return Item
                  { itemId = fromSql iid
                  , itemDescription = fromSql ide
                  , itemCreatedAt = fromSql ica
                  , itemPriority = fromSql ipr
                  , itemChildren = sortItems children
                  }

      sortItems :: [Item] -> [Item]
      sortItems = sortBy $ \x y -> comparing itemPriority x y

data Item = Item
   { itemId :: Integer
   , itemDescription :: String
   , itemCreatedAt :: LocalTime
   , itemPriority :: Integer
   , itemChildren :: [Item]
   }
   deriving(Show, Eq)

executeInitCommand :: Config -> TodoCommand -> IO ()
executeInitCommand config showFlags = undefined

executeAddCommand :: Config -> TodoCommand -> IO ()
executeAddCommand config addFlags = do
   d <- getData
   case d of
      Nothing -> putStrLn "Need a comment and priority to add a new item, or reacting to early termination."
      Just (comment, pri, tags) -> do
         conn <- getDatabaseConnection
         run conn addInsertion [toSql comment, toSql (parent addFlags), toSql pri]
         itemId <- getLastId conn
         unless (null tags) $ do
            tagIds <- findOrCreateTags conn tags
            insertStatement <- prepare conn "INSERT INTO tag_map (item_id, tag_id) VALUES (?,?)"
            mapM_ (execute insertStatement) [[toSql itemId, tag] | tag <- map toSql tagIds]
         commit conn
         disconnect conn
         putStrLn $ "Added item " ++ show itemId ++ " successfully."
   where 
      findOrCreateTags :: (IConnection c) => c -> [String] -> IO [Integer]
      findOrCreateTags conn = mapM findOrCreateTag
         where
            findOrCreateTag :: String -> IO Integer
            findOrCreateTag tag = do
               res <- return . tryGetId =<< quickQuery' conn "select id from tags where tag_name = ?" [toSql tag]
               case res of
                  Just x -> return x
                  Nothing -> do
                     run conn "INSERT INTO tags (tag_name) VALUES (?)" [toSql tag]
                     getLastId conn

            tryGetId :: [[SqlValue]] -> Maybe Integer
            tryGetId [[x]] = Just (fromSql x)
            tryGetId _ = Nothing

      getData :: IO (Maybe (String, String, [String]))
      getData = runInputT defaultSettings (runMaybeT getDataHelper)
         where
            getDataHelper :: MaybeT (InputT IO) (String, String, [String])
            getDataHelper = do
               Just description <- lift $ getInputLine "comment> "
               guard (not $ null description)
               Just pri <- lift $ getInputLine "priority> "
               guard (not $ null pri)
               Just tags <- lift $ getInputLine "tags> "
               return (description, pri, words tags)

      putStrFlush :: String -> IO ()
      putStrFlush s = putStr s >> hFlush stdout 

      addInsertion :: String
      addInsertion = "INSERT INTO items (description, created_at, parent_id, priority)" ++ 
                     "VALUES (?, datetime() ,?,?)"

getLastId :: (IConnection c) => c -> IO Integer
getLastId conn = return . extractId =<< quickQuery' conn "select last_insert_rowid()" []

extractId :: [[SqlValue]] -> Integer
extractId [[x]] = fromSql x
extractId _ = error "Could not parse id result."

executeEditCommand :: Config -> TodoCommand -> IO ()
executeEditCommand config editCommand = do
   
   

executeDoneCommand :: Config -> TodoCommand -> IO ()
executeDoneCommand _ _ = unimplemented

unimplemented = putStrLn "Not Implemented Yet"

separateBy :: Char -> String -> Maybe [String]
separateBy sep input = case parse parseCommas "(unknown)" input of
   Left _ -> Nothing
   Right x -> Just x
   where
      parseCommas :: Parser [String]
      parseCommas = sepBy (many1 (noneOf [sep])) (char sep)

separateCommas :: String -> Maybe [String]
separateCommas = separateBy ','

getDatabaseConnection :: IO Connection
getDatabaseConnection = connectSqlite3 ".htodo.db"
