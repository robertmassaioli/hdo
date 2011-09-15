import Control.Monad (unless)

import System.Directory
import System.Exit 

import TodoArguments
import Configuration

import Init
import Show
import Add
import Edit
import Done

main = do
   preConfig <- defaultConfig
   command <- getCommandInput
   let config = getUpdatedConfig command preConfig
   setupAppDir config
   executeCommand config command

getUpdatedConfig :: TodoCommand -> Config -> Config
getUpdatedConfig command original = 
   case databaseFile command of
      Nothing -> original
      Just file -> original { defaultDatabaseName = file }

setupAppDir :: Config -> IO ()
setupAppDir config = do
   appDirExists <- doesDirectoryExist (defaultAppDirectory config)
   unless appDirExists $ do 
      createDirectory (defaultAppDirectory config)
      putStrLn $ "Created app data directory: " ++ defaultAppDirectory config

executeCommand :: Config -> TodoCommand -> IO ()
executeCommand c x@(Show {})     = executeShowCommand c x
executeCommand c x@(Init {})     = executeInitCommand c x
executeCommand c x@(Add {})      = executeAddCommand c x
executeCommand c x@(Edit {})     = executeEditCommand c x
executeCommand c x@(Done {})     = executeDoneCommand c x
--executeCommand c x@(Rename {})   = executeRenameCommand c x
--executeCommand c x@(Move {})     = executeMoveCommand c x
executeCommand c x = do
   putStrLn "Was unable to parse the option that I was given."
   exitWith $ ExitFailure 2

executeInitCommand :: Config -> TodoCommand -> IO ()
executeInitCommand config initFlags = createDatabase initFlags config
