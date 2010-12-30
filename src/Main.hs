{-# LANGUAGE DeriveDataTypeable #-}
import System.Console.CmdArgs.Implicit
import Control.Monad

data TodoCommand = 
            Init 
            | Add 
            | Done 
            | Edit { editIds :: [Integer] }
            | Help
            deriving(Eq, Show, Data, Typeable)

initMode = Init &= help "Init the todo."
addMode = Add &= help "Add one or more todo items."
doneMode = Done &= help "Mark one or more todo items as done."
editMode = Edit {
               editIds = [] &= args
            }
            &= help "Edit one or more todo items."
helpMode = Help &= help "Show global help for htodo."

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
               &= summary "htodo v0.1"

main = cmdArgsRun arguments >>= print
