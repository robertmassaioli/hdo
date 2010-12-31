{-# LANGUAGE DeriveDataTypeable #-}
import System.Console.CmdArgs.Implicit
import Control.Monad

import Data.Maybe (catMaybes)
import Data.List (intersperse)

import Text.ParserCombinators.Parsec

data TodoCommand 
            = Show
               { showEntireHierarchy :: Bool
               , showDone :: Bool
               , showUsingFilter :: Maybe String
               , showFilterExtra :: Maybe String
               }
            | Init 
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

showMode :: TodoCommand
showMode = Show 
               { showEntireHierarchy = def 
                  &= explicit
                  &= name "a" &= name "all"
                  &= help "Show the entire hierarchy of the todo list items."
               , showDone = def
                  &= explicit
                  &= name "d" &= name "done"
                  &= help "Also show the items that are done."
               , showUsingFilter = def
                  &= explicit
                  &= name "filter"
                  &= typ "f1,f2,f3,..."
                  &= help ("Sometimes the normal command line options are not enough; this allows you to "
                     ++ "specify a filter. The default is obviously to filter on nothing and thus show "
                     ++ "everything.")
               , showFilterExtra = def
                  &= args
                  &= typ "filters..."
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
   [ showMode &= auto
   , initMode
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

main = do
   command <- cmdArgsRun arguments
   print command
   executeCommand command

executeCommand :: TodoCommand -> IO ()
executeCommand x@(Show {}) = executeShowCommand x
executeCommand x@(Init {}) = executeShowCommand x
executeCommand x@(Add {}) = executeShowCommand x
executeCommand x@(Edit {}) = executeShowCommand x
executeCommand x@(Done {}) = executeShowCommand x

data FilterType 
   = FilterTypeAdd 
   | FilterTypeEquals 
   | FilterTypeRemove
   | FilterTypeDefault
   deriving(Eq, Show)

data Filter 
   = FilterAll 
      { filterType :: FilterType 
      }
   | FilterChildren
      { filterType :: FilterType 
      }
   | FilterDone
      { filterType :: FilterType 
      }
   | FilterPriority
      { filterType :: FilterType 
      , filterPriority :: Integer
      }
   | FilterIndex
      { filterType :: FilterType 
      , filterRange :: [Range Integer]
      }
   | FilterRegex
      { regexString :: String
      }
   deriving(Eq, Show)

data Range a
   = SingletonRange a
   | SpanRange a a
   deriving(Eq, Show)

executeShowCommand :: TodoCommand -> IO ()
executeShowCommand (Show sa sd f_one f_two) = do
   putStrLn "Should be showing stuff now."
   print filter_str
   print $ parse parseFilters "(unknown)" filter_str
   where
      filter_str = concat . intersperse "," . catMaybes $ [f_one, f_two]

      split :: String -> [String]
      split [] = []
      split xs = takeWhile (/= ',') xs : case dropWhile (/= ',') xs of
                                             [] -> []
                                             x -> split . tail $ x

parseFilters :: CharParser st [Filter]
parseFilters = sepBy parseFilter (char ',')

parseFilter :: CharParser st Filter
parseFilter 
   = try (templateFilter "all" FilterAll)
   <|> try (templateFilter "children" FilterChildren)
   <|> try (templateFilter "done" FilterDone)
   <|> indexRanges
   where
      templateFilter :: String -> (FilterType -> Filter) -> CharParser st Filter
      templateFilter name constructor = do
         ft <- parseFilterType
         string name
         return $ constructor ft

      indexRanges :: CharParser st Filter
      indexRanges = do
         ft <- parseFilterType
         ranges <- sepBy indexRange (char '.')
         return $ FilterIndex { filterType = ft, filterRange = ranges }
         where
            indexRange :: CharParser st (Range Integer)
            indexRange = do
               first <- many1 digit
               decide first

            decide :: String -> CharParser st (Range Integer)
            decide first = 
               (do
                  char '-'
                  second <- many1 digit 
                  return (SpanRange (read first) (read second))
               )
               <|> return (SingletonRange (read first))

      parseFilterType :: CharParser st FilterType
      parseFilterType 
         = (char '+' >> return FilterTypeAdd)
         <|> (char '-' >> return FilterTypeRemove)
         <|> (char '=' >> return FilterTypeEquals)
         <|> return FilterTypeDefault

