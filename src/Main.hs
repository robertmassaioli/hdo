{-# LANGUAGE DeriveDataTypeable #-}
import System.Console.CmdArgs.Implicit
import Control.Monad

data TodoCommand = 
            Init 
               { userLevel :: Bool 
               }
            | Add 
               { fromFile :: Maybe FilePath 
               , priority :: Maybe Integer
               }
            | Done 
            | Edit { editIds :: [Integer] }
            | Help
            deriving(Eq, Show, Data, Typeable)

initMode :: TodoCommand
initMode = Init 
               { userLevel = False  
                  &= explicit
                  &= name "for-user"
                  &= help ("Specifies wether or not this is for the user or the current directory." 
                            ++ " Current directory is used by default.")
               }
            &= help "Initialise the todo."
            &= details 
               [ "This function allows you to create a todo list database in either the current directory "
               ++ "or in a location that is good for the current user."
               ]

addMode :: TodoCommand
addMode = Add 
            { fromFile = Nothing &= explicit 
                                 &= name "from-file" 
                                 &= help "Add a bunch of todo items from a file."
                                 &= typ "<file_name>"
            , priority = Nothing &= name "priority" 
                                 &= help "What priority level is this todo item." 
                                 &= typ "[1-9]"
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
            &= help "Mark one or more todo items as done."
            &= details
            [ "Eventually you will want to mark items as done. This allows you to mark items as done and lets "
            ++ "you make comments on them once you are. You can mark more than one item as done at a time which "
            ++ "greatly speeds up the time it takes to mark off items."
            ]

editMode :: TodoCommand 
editMode = Edit {
               editIds = [] &= args
            }
            &= help "Edit one or more todo items."
            &= details
            [ "Editing a todo item is a natural part of the process of managing these items. In this mode you "
            ++ "can edit the description, add/remove tags, parent/reparent todo items and otherwise do any kind "
            ++ "of editing of the todo items that you would like."
            ]

combinedModes :: TodoCommand
combinedModes = modes 
   [ initMode
   , addMode
   , doneMode
   , editMode
   ]

arguments :: Mode (CmdArgs TodoCommand)
arguments = cmdArgsMode $ 
               combinedModes 
               &= help "The Haskell Todo" 
               &= helpArg [explicit, name "h", name "help"]
               &= program "htodo" 
               &= summary "htodo v0.1 - By Robert Massaioli"

main = cmdArgsRun arguments >>= print
