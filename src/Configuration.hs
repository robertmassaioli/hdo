module Configuration 
   ( Config(..)
   , defaultConfig
   , getDatabaseConnection
   , findHTodoDatabase
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
      { defaultDatabaseName = "htodo.db"
      , defaultAppDirectory = appDir
      , defaultSchemaDir = "./schema"
      }

getDatabaseConnection :: Config -> IO (Maybe Connection)
getDatabaseConnection config = do
   path <- findHTodoDatabase config 
   case path of
      Nothing -> putStrLn "Could not find hTodo database in path." >> return Nothing
      Just validPath -> do
         putStrLn $ "Using database: " ++ validPath
         connectSqlite3 validPath >>= return . Just

findHTodoDatabase :: Config -> IO (Maybe FilePath)
findHTodoDatabase config = do
   current <- getCurrentDirectory
   home <- getHomeDirectory
   searchPathForFile config . generateSearchPath $ makeRelative home current

generateSearchPath :: FilePath -> [FilePath]
generateSearchPath a = a : if takeDirectory a /= a 
                              then generateSearchPath (takeDirectory a) 
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
      potentialLocations = map (</> hiddenFileName) paths ++ defaultLocation
         where 
            defaultLocation = [defaultAppDirectory config </> defaultDatabaseName config]
            hiddenFileName = '.' : defaultDatabaseName config

      makeTuple :: (a -> b) -> a -> (a, b)
      makeTuple f a = (a, f a)
