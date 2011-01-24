import Control.Monad (unless, liftM)
import Control.Monad.Reader

import Control.Monad.Maybe
import Data.Maybe (catMaybes)

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Time (LocalTime)
import Data.List(sortBy, (\\), nub, intercalate)
import Data.Ord(comparing)
import Data.Maybe(fromMaybe)

import Data.Char (isDigit)

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
import Range

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

data EventTypes = EventAdd | EventEdit | EventDone | EventRemove
                deriving(Enum, Eq, Show)

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
      filter_str = intercalate "," . catMaybes $ [showUsingFilter showFlags, showFilterExtra showFlags]

      generateQuery :: [String] -> String
      generateQuery [] = "select i.* from items i where 1 = 1"
      generateQuery xs = queryLeft ++ " AND (" 
                         ++ (intercalate " OR " . map (\s -> "t.tag_name = \"" ++ s ++ "\"") $ xs)
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
executeInitCommand config showFlags = unimplemented

executeAddCommand :: Config -> TodoCommand -> IO ()
executeAddCommand config addFlags = do
   d <- getData
   case d of
      Nothing -> putStrLn "Need a comment and priority to add a new item, or reacting to early termination."
      Just (comment, pri, tags) -> do
         conn <- getDatabaseConnection
         run conn addInsertion [toSql comment, toSql (parent addFlags), toSql pri]
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

findOrCreateTags :: (IConnection c) => c -> Integer -> [String] -> IO [Integer]
findOrCreateTags conn itemId = mapM findOrCreateTag
   where
      findOrCreateTag :: String -> IO Integer
      findOrCreateTag tag = do
         res <- return . tryGetId =<< quickQuery' conn "select id from tags where tag_name = ?" [toSql tag]
         case res of
            Just x -> return x
            Nothing -> do
               run conn "INSERT INTO tags (tag_name, created_at) VALUES (?, datetime())" [toSql tag]
               getLastId conn

      tryGetId :: [[SqlValue]] -> Maybe Integer
      tryGetId [[x]] = Just (fromSql x)
      tryGetId _ = Nothing

getLastId :: (IConnection c) => c -> IO Integer
getLastId conn = return . extractId =<< quickQuery' conn "select last_insert_rowid()" []

extractId :: [[SqlValue]] -> Integer
extractId [[x]] = fromSql x
extractId _ = error "Could not parse id result."

executeEditCommand :: Config -> TodoCommand -> IO ()
executeEditCommand config editCommand = do
   conn <- getDatabaseConnection
   mapM_ (editSingleId conn) $ editIds editCommand
   commit conn
   disconnect conn
      where
         editSingleId :: (IConnection c) => c -> Integer -> IO ()
         editSingleId conn id = do
            d <- runMaybeT $ getEditData conn id
            case d of
               Nothing -> putStrLn $ "Could not find data for id: " ++ show id
               Just oldData@(_,_,oldTags) -> do 
                  newData <- runInputT defaultSettings (runMaybeT (askEditQuestions oldData))
                  case newData of
                     Nothing -> putStrLn "Invalid input or early termination."
                     Just (desc, pri, tags) -> do 
                        -- TODO create an edit event here to log the change
                        run conn updateItem [toSql desc, toSql pri, toSql id]
                        cs <- prepare conn createStatement
                        ds <- prepare conn deleteStatement
                        findOrCreateTags conn id (tags \\ oldTags) >>= mapM_ (createTagMapping cs id)
                        getTagMapIds conn id (oldTags \\ tags) >>= mapM_ (deleteTagMapping ds id)
                        -- please note that we intentionally do not delete tags here; just the
                        -- mappings, we leave them around for later use. The 'htodo clean' or maybe
                        -- 'htodo gc' command will do that cleanup I think.
            where
               updateItem = "UPDATE items SET description = ?, priority = ? where id = ?"

               getTagMapIds :: (IConnection c) => c -> Integer -> [String] -> IO [Integer]
               getTagMapIds _ _ [] = return []
               getTagMapIds conn itemId tags = do
                  quickQuery' conn theQuery [toSql itemId] >>= return . map fromSql . concat
                  where
                     theQuery = "SELECT tm.tag_id from tag_map tm, tags t where t.id = tm.tag_id and (" ++ tagOrList ++ ") and tm.item_id = ?"
                     tagOrList = intercalate " OR " $ map (\s -> "t.tag_name = \"" ++ s ++ "\"") tags

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
                  Just newDesc <- lift $ getInputLineDefault desc "comment> "
                  guard (not $ null newDesc)
                  Just newPri <- lift $ getInputLineDefault (show pri) "priority> "
                  guard (not $ null newPri)
                  guard (and $ map isDigit newPri) -- the priority must be a digit
                  Just newTags <- lift $ getInputLineDefault (unwords tags) "tags> "
                  return (newDesc, read newPri, nub . words $ newTags)
                  

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

{-
parseRanges :: String -> Maybe [Range]
parseRanges input =
   case parse parseRangeHelper "(ranges)" input of
      Left _ -> Nothing
      Right x -> Just x
   where
      parseRangeHelper :: Parser [Range]
      parseRangeHelper = sepBy (many parseRange) (char ',')

      parseRange :: Parser Range
      parseRange = try (parseRangeSpan) <|> parseRangeSingleton <?> "Could not parse valid range."

      parseRangeSpan :: Parser Range
      parseRangeSpan = do
         first <- many digit
         char '-'
         second <- many digit
         return $ RangeSpan first second

      parseRangeSingleton :: Parser Range
      parseRangeSingleton = many digit
-}
