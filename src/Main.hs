import Control.Monad (unless, liftM)
import Control.Monad.Reader
import Data.Maybe (catMaybes)

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Time (LocalTime)
import Data.List(sortBy, intersperse)
import Data.Ord(comparing)

import Filter
import TodoArguments

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token

import Text.Show.Pretty

import System.Directory
import System.FilePath
import System.IO

prettyShow :: (Show a) => a -> IO ()
prettyShow = putStrLn . ppShow

data Config = Config 
   { defaultDatabaseName :: String
   , defaultAppDirectory :: FilePath
   , defaultSchemaDir :: FilePath
   }
   deriving(Show)

defaultConfig :: IO Config
defaultConfig = do 
   appDir <- getAppUserDataDirectory "htodo"
   return Config
      { defaultDatabaseName = "htodo.db"
      , defaultAppDirectory = appDir
      , defaultSchemaDir = "./schema"
      }

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
   case showUsingTags showFlags of
      Nothing -> putStrLn "No Tags."
      Just x -> print $ separateCommas x
   unless (filter_str == "") $ print (getFilters filter_str)
   conn <- getDatabaseConnection
   getTodoItems conn >>= displayItems
   disconnect conn
   where
      filter_str = concat . intersperse "," . catMaybes $ [showUsingFilter showFlags, showFilterExtra showFlags]

displayItems :: [Item] -> IO ()
displayItems = mapM_ (displayItemHelper 0)
   where
      displayItemHelper :: Int -> Item -> IO ()
      displayItemHelper level item = do
         putStr $ replicate (level * 3 + 1) ' '
         putStr $ show (itemId item) ++ ". "
         putStrLn $ itemDescription item
         mapM_ (displayItemHelper (level + 1)) (itemChildren item)
   
getTodoItems :: (IConnection c) => c -> IO [Item]
getTodoItems conn = do
   topLevels <- quickQuery' conn "select i.* from items i where i.parent_id is ?" [SqlNull]
   fmap sortItems $ mapM createChild topLevels
   where
      createChild :: [SqlValue] -> IO Item
      createChild [iid, ide, ica, _, ipr] = do
         let this_id = fromSql iid :: Integer
         children <- mapM createChild =<< quickQuery' conn ("select i.* from items i where i.parent_id = " ++ show this_id) []
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
executeAddCommand config showFlags = do
   (comment, pri, tags) <- getData
   if (null comment) || (null pri)
      then
         putStrLn "Need a comment and priority to add a new item."
      else do
         conn <- getDatabaseConnection
         run conn addInsertion [toSql comment, SqlNull, toSql pri]
         itemId <- getLastId conn
         print itemId
         unless (null tags) $ do
            tagIds <- findOrCreateTags conn tags
            insertStatement <- prepare conn "INSERT INTO tag_map (item_id, tag_id) VALUES (?,?)"
            mapM_ (execute insertStatement) [[toSql itemId, tag] | tag <- map toSql tagIds]
         commit conn
         disconnect conn
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

      getData :: IO (String, String, [String])
      getData = do
         putStrFlush "comment> "
         description <- getLine
         if (null description) 
            then 
               return ("", "", [])
            else do
               putStrFlush "priority> "
               priority <- getLine
               if (null priority) 
                  then
                     return ("", "", [])
                  else do
                     putStrFlush "tags> "
                     tags <- getLine
                     return (description, priority, words tags)

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
executeEditCommand _ _ = unimplemented

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
