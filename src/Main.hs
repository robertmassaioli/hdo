import Control.Monad (unless, liftM)
import Control.Monad.Reader
import Data.Maybe (catMaybes)
import Data.List (intersperse)

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Time (LocalTime)
import Data.List(sortBy)

import Filter
import TodoArguments

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token

import Text.Show.Pretty

import System.Directory
import System.FilePath

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
   return $ Config
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
   executeCommand command

setupAppDir :: ReaderT Config IO ()
setupAppDir = do
   config <- ask
   liftIO $ do
      appDirExists <- doesDirectoryExist (defaultAppDirectory config)
      unless appDirExists $ do
         createDirectory (defaultAppDirectory config)
         createDatabase 
            ((defaultAppDirectory config) </> (defaultDatabaseName config)) 
            (defaultSchemaDir config </> create_file)
   where
      create_file :: FilePath
      create_file = "create_database.sqlite3.read"

createDatabase :: FilePath -> FilePath -> IO ()
createDatabase databaseFile schemaFile = do
   conn <- connectSqlite3 databaseFile
   putStrLn databaseFile
   createCommands <- readFile schemaFile
   prettyShow $ map init . lines $ createCommands
   withTransaction conn (createEverything createCommands)
   disconnect conn
   where
      createEverything :: (IConnection conn) => String -> conn -> IO ()
      createEverything createCommands conn = mapM_ (quickQuery'' conn []) . map init . lines $ createCommands
         where quickQuery'' conn qry reps = quickQuery' conn reps qry 

executeCommand :: TodoCommand -> IO ()
executeCommand x@(Show {}) = runReaderT executeShowCommand x
executeCommand x@(Init {}) = executeInitCommand x
executeCommand x@(Add {}) = executeAddCommand x
executeCommand x@(Edit {}) = executeEditCommand x
executeCommand x@(Done {}) = executeDoneCommand x

executeShowCommand :: ReaderT TodoCommand IO ()
executeShowCommand = do
   showFlags <- ask
   case showUsingTags showFlags of
      Nothing -> liftIO $ putStrLn "No Tags."
      Just x -> liftIO $ print $ separateCommas x
   liftIO $ unless (filter_str showFlags == "") $ print (getFilters $ filter_str showFlags)
   conn <- liftIO $ connectSqlite3 ".htodo.db"
   liftIO $ getTodoItems conn >>= displayItems
   liftIO $ disconnect conn
   where
      filter_str showFlags = concat . intersperse "," . catMaybes $ [showUsingFilter showFlags, showFilterExtra showFlags]

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
   mapM createChild topLevels >>= return . sortItems
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
      sortItems = sortBy $ \x y -> compare (itemPriority x) (itemPriority y)

data Item = Item
   { itemId :: Integer
   , itemDescription :: String
   , itemCreatedAt :: LocalTime
   , itemPriority :: Integer
   , itemChildren :: [Item]
   }
   deriving(Show, Eq)

executeInitCommand :: TodoCommand -> IO ()
executeInitCommand _ = unimplemented

executeAddCommand :: TodoCommand -> IO ()
executeAddCommand _ = unimplemented

executeEditCommand :: TodoCommand -> IO ()
executeEditCommand _ = unimplemented

executeDoneCommand :: TodoCommand -> IO ()
executeDoneCommand _ = unimplemented

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
