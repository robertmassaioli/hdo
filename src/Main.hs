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
            , priority = Nothing &= name "priority" 
                                 &= help "What priority level is this todo item." 
            }
            &= help "Add one or more todo items."

doneMode :: TodoCommand
doneMode = Done 
            &= help "Mark one or more todo items as done."

editMode :: TodoCommand 
editMode = Edit {
               editIds = [] &= args
            }
            &= help "Edit one or more todo items."

helpMode :: TodoCommand 
helpMode = Help 
            &= help "Show global help for htodo."

combinedModes :: TodoCommand
combinedModes = modes 
   [ initMode
   , addMode
   , helpMode &= auto
   , doneMode
   , editMode
   ]

arguments :: Mode (CmdArgs TodoCommand)
arguments = cmdArgsMode $ 
               combinedModes 
               &= help "The Haskell Todo" 
               &= program "htodo" 
               &= summary "htodo v0.1 - By Robert Massaioli"

main = cmdArgsRun arguments >>= print
