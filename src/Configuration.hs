module Configuration 
   ( Config(..)
   , defaultConfig
   , getDatabaseConnection
   , findHdoDatabase
   , defaultDatabaseLocation
   , hiddenFileName
   ) where

import System.Directory
import System.FilePath
import Database.HDBC.Sqlite3

import TodoArguments
import Util

data Config = Config 
   { defaultDatabaseName :: String
   , defaultAppDirectory :: FilePath
   }
   deriving(Show)

defaultConfig :: IO Config
defaultConfig = do 
   appDir <- getAppUserDataDirectory "hdo"
   return Config
      { defaultDatabaseName = ".hdo.db"
      , defaultAppDirectory = appDir
      }

getDatabaseConnection :: Config -> TodoCommand -> IO (Maybe Connection)
getDatabaseConnection config command = do
   path <- if userLevel command
               then findHomeDatabase
               else findHdoDatabase config 
   case path of
      Nothing -> putStrLn "Could not find hDo database in path. Maybe you should try 'hdo init' to start a new todo in the current directory?" >> return Nothing
      Just validPath -> do
         putStrLn $ "Using database: " ++ validPath
         putNewline
         fmap Just $ connectSqlite3 validPath
   where
      findHomeDatabase :: IO (Maybe FilePath)
      findHomeDatabase = searchPathForFile config []

findHdoDatabase :: Config -> IO (Maybe FilePath)
findHdoDatabase config = do
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
   case filter snd $ zip potentialLocations results of
      [] -> return Nothing
      xs -> return . Just . fst . head $ xs
   where    
      potentialLocations :: [FilePath]
      potentialLocations = map (</> defaultDatabaseName config) paths ++ [defaultDatabaseLocation config]

defaultDatabaseLocation :: Config -> FilePath
defaultDatabaseLocation config = defaultAppDirectory config </> tail (defaultDatabaseName config)

hiddenFileName :: Config -> FilePath
hiddenFileName config = '.' : defaultDatabaseName config
