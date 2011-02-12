module Configuration 
   ( Config(..)
   , defaultConfig
   , getDatabaseConnection
   , findHTodoDatabase
   , defaultDatabaseLocation
   , hiddenFileName
   ) where

import System.Directory
import System.FilePath
import Database.HDBC.Sqlite3

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
      { defaultDatabaseName = ".htodo.db"
      , defaultAppDirectory = appDir
      , defaultSchemaDir = "./schema"
      }

getDatabaseConnection :: Config -> IO (Maybe Connection)
getDatabaseConnection config = do
   path <- findHTodoDatabase config 
   case path of
      Nothing -> putStrLn "Could not find hTodo database in path. Maybe you should try 'htodo init' to start a new todo in the current directory?" >> return Nothing
      Just validPath -> do
         putStrLn $ "Using database: " ++ validPath
         connectSqlite3 validPath >>= return . Just

findHTodoDatabase :: Config -> IO (Maybe FilePath)
findHTodoDatabase config = do
   current <- getCurrentDirectory
   home <- getHomeDirectory
   searchPathForFile config $ generateSearchPath home current

generateSearchPath :: FilePath -> FilePath -> [FilePath]
generateSearchPath home initial = go initial
   where 
      go :: FilePath -> [FilePath]
      go a = a : if takeDirectory a /= a && a /= home
                     then go $ takeDirectory a
                     else []

-- it adds on the default path for you
searchPathForFile :: Config -> [FilePath] -> IO (Maybe FilePath)
searchPathForFile config paths = do
   results <- mapM doesFileExist potentialLocations
   case filter (\s -> snd s == True) $ zip potentialLocations results of
      [] -> return Nothing
      xs -> return . Just . fst . head $ xs
   where    
      potentialLocations :: [FilePath]
      potentialLocations = map (</> defaultDatabaseName config) paths ++ [defaultDatabaseLocation config]

defaultDatabaseLocation :: Config -> FilePath
defaultDatabaseLocation config = defaultAppDirectory config </> (tail $ defaultDatabaseName config)

hiddenFileName :: Config -> FilePath
hiddenFileName config = '.' : defaultDatabaseName config
