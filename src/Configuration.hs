module Configuration where

import System.Directory

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
