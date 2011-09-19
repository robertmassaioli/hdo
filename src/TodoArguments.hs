{-# LANGUAGE DeriveDataTypeable #-}

module TodoArguments (
   TodoCommand(..),
   getCommandInput
   ) where

import System.Console.CmdArgs.Implicit

data TodoCommand 
            = Show 
               { userLevel :: Bool
               , showEntireHierarchy :: Bool
               , showDone :: Bool
               , showUsingFilter :: Maybe String
               , showUsingTags :: Maybe String
               , showFilterExtra :: Maybe String
               , databaseFile :: Maybe FilePath
               }
            | Init 
               { userLevel :: Bool 
               , databaseFile :: Maybe FilePath
               }
            | Add 
               { userLevel :: Bool
               , fromFile :: Maybe FilePath     -- Not Implemented Yet
               , priority :: Maybe Integer
               , databaseFile :: Maybe FilePath
               , listPath :: Maybe String
               }
            | Done 
               { userLevel :: Bool
               , databaseFile :: Maybe FilePath
               , doneRanges :: String
               }
            | Edit 
               { userLevel :: Bool
               , editRanges :: String 
               , databaseFile :: Maybe FilePath
               }
            | Rename       -- This renames one list to another
               { userLevel :: Bool
               , fromListPath :: [String]
               , toListPath :: String
               , databaseFile :: Maybe FilePath
               }
            | Move         -- This moves an item from one list into another
               { userLevel :: Bool
               , itemRanges :: [String]
               , toListPath :: String
               , databaseFile :: Maybe FilePath
               }
            deriving(Eq, Show, Data, Typeable)

getCommandInput :: IO TodoCommand
getCommandInput = cmdArgsRun todoArguments

todoArguments :: Mode (CmdArgs TodoCommand)
todoArguments = cmdArgsMode $ 
               combinedModes 
               &= help "The program who's to do list is to do your todo's." 
               &= helpArg [explicit, name "h", name "help"]
               &= program "htodo" 
               &= summary "htodo v0.1 - By Robert Massaioli"

combinedModes :: TodoCommand
combinedModes = modes 
   [ showMode &= auto
   , initMode
   , addMode
   , doneMode
   , editMode
   , renameMode
   , moveMode
   ]

showMode :: TodoCommand
showMode = Show 
            { userLevel = False  
               &= explicit
               &= name "for-user" &= name "u"
               &= help ("Specifies whether or not this is for the user or the current directory." 
                         ++ " Current directory is used by default.")
            , showEntireHierarchy = def 
               &= explicit
               &= name "a" &= name "all"
               &= help "Show the entire hierarchy of the todo list items."
            , showDone = def
               &= explicit
               &= name "done"
               &= help "Also show the items that are done."
            , showUsingFilter = def
               &= explicit
               &= name "filter"
               &= typ "f1,f2,f3,..."
               &= help ("Sometimes the normal command line options are not enough; this allows you to "
                  ++ "specify a filter. The default is obviously to filter on nothing and thus show "
                  ++ "everything.")
            , showUsingTags = def
               &= explicit
               &= name "t" &= name "tags"
               &= typ "tag1,tag2,tag3,..."
               &= help ("Only data that are tagged with the tags that you supply will be included. If "
                  ++ "no tag was provided when the todo item was added then it will be tagged 'main' by "
                  ++ "default")
            , showFilterExtra = def
               &= args
               &= typ "filters..."
            , databaseFile = def 
               &= explicit
               &= name "d" &= name "database"
               &= typ "./path/to/database.db"
               &= help "This is the path to the database file."
            }
            &= help "Show todo items from the database."
            &= details
            [ "You are eventually going to want to display items from the database and this is what will "
            ++ "allow you to do exactly that. This is the default mode that htodo runs in."
            ]

initMode :: TodoCommand
initMode = Init 
            { userLevel = False  
               &= explicit
               &= name "for-user" &= name "u"
               &= help ("Specifies whether or not this is for the user or the current directory." 
                         ++ " Current directory is used by default.")
            , databaseFile = def
               &= explicit
               &= name "d" &= name "database"
               &= typ "./path/to/database.db"
               &= help "This is the path to the database file."
            }
            &= help "Initialise the todo database and config."
            &= details 
               [ "This function allows you to create a todo list database in either the current directory "
               ++ "or in a location that is good for the current user."
               ]

addMode :: TodoCommand
addMode = Add 
            { userLevel = False  
               &= explicit
               &= name "for-user" &= name "u"
               &= help ("Specifies whether or not this is for the user or the current directory." 
                         ++ " Current directory is used by default.")
            , fromFile = Nothing &= explicit 
                                 &= name "from-file" 
                                 &= help "Add a bunch of todo items from a file."
                                 &= typ "<file_name>"
            , listPath = Nothing &= args
                                 &= typ "list/path"
            , priority = Nothing &= name "priority" 
                                 &= help "What priority level is this todo item." 
                                 &= typ "[1-9]"
            , databaseFile = def
               &= explicit
               &= name "d" &= name "database"
               &= typ "./path/to/database.db"
               &= help "This is the path to the database file."
            }
            &= help "Add one or more todo items."
            &= details
            [ "You should be able to add todo items to this easily and efficiently but there are many different "
            ++ "ways in which that scenario could arise. You could simply want to type it in on the command "
            ++ "line, or you could be using grep to get them out of your source code or you could even be "
            ++ "getting them from some file you just had lying around or that you quickly jotted down while "
            ++ "you were away from your computer and the internet. The possibilities are endless."
            , "But either way this should allow you to add items, irrecspective of their source, to your htodo "
            ++ "database."
            ]

doneMode :: TodoCommand
doneMode = Done 
            { userLevel = False  
               &= explicit
               &= name "for-user" &= name "u"
               &= help ("Specifies whether or not this is for the user or the current directory." 
                         ++ " Current directory is used by default.")
            , doneRanges = def &= args
            , databaseFile = def
               &= explicit
               &= name "d" &= name "database"
               &= typ "./path/to/database.db"
               &= help "This is the path to the database file."
            }
            &= help "Mark one or more todo items as done."
            &= details
            [ "Eventually you will want to mark items as done. This allows you to mark items as done and lets "
            ++ "you make comments on them once you are. You can mark more than one item as done at a time which "
            ++ "greatly speeds up the time it takes to mark off items."
            ]

editMode :: TodoCommand 
editMode = Edit 
            { userLevel = False  
               &= explicit
               &= name "for-user" &= name "u"
               &= help ("Specifies whether or not this is for the user or the current directory." 
                         ++ " Current directory is used by default.")
            , editRanges = [] &= args
            , databaseFile = def 
               &= explicit
               &= name "d" &= name "database"
               &= typ "./path/to/database.db"
               &= help "This is the path to the database file."
            }
            &= help "Edit one or more todo items."
            &= details
            [ "Editing a todo item is a natural part of the process of managing these items. In this mode you "
            ++ "can edit the description, add/remove tags, parent/reparent todo items and otherwise do any kind "
            ++ "of editing of the todo items that you would like."
            ]

renameMode :: TodoCommand
renameMode = Rename
   { userLevel = False  
      &= explicit
      &= name "for-user" &= name "u"
      &= help ("Specifies whether or not this is for the user or the current directory." 
                ++ " Current directory is used by default.")
   , fromListPath = [] 
      &= args
      &= typ "FROM_LIST"
   , toListPath = "" 
      &= argPos 0
      &= typ "TO_LIST"
   , databaseFile = def 
      &= explicit
      &= name "d" &= name "database"
      &= typ "./path/to/database.db"
      &= help "This is the path to the database file."
   }
   &= help "Rename one or more lists to the same list name."
   &= details
   [ "Occasionally you will want to rename lists into something else and move them arround. This "
   ++ "command will let you do that. This command uses a many-to-one relationship when renaming "
   ++ "lists."
   ]

moveMode :: TodoCommand
moveMode = Move
   { userLevel = False  
      &= explicit
      &= name "for-user" &= name "u"
      &= help ("Specifies whether or not this is for the user or the current directory." 
                ++ " Current directory is used by default.")
   , itemRanges = [] 
      &= args
      &= typ "ITEM_RANGES"
   , toListPath = "" 
      &= argPos 0
      &= typ "LIST"
   , databaseFile = def 
      &= explicit
      &= name "d" &= name "database"
      &= typ "./path/to/database.db"
      &= help "This is the path to the database file."
   }
   &= help "Move one or more items to another list."
   &= details
   [ "Moving items between lists is an important task: this command lets you do exactly that by moving "
   ++ "as many items as you want to another list. You can pull in the items from multiple other lists and "
   ++ "move them all into the one list."
   ]
