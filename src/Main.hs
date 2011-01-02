import Control.Monad (unless)
import Data.Maybe (catMaybes)
import Data.List (intersperse)

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Time (LocalTime)

import Filter
import TodoArguments

main = do
   command <- getCommandInput
   print command
   executeCommand command

executeCommand :: TodoCommand -> IO ()
executeCommand x@(Show {}) = executeShowCommand x
executeCommand x@(Init {}) = executeInitCommand x
executeCommand x@(Add {}) = executeAddCommand x
executeCommand x@(Edit {}) = executeEditCommand x
executeCommand x@(Done {}) = executeDoneCommand x

executeShowCommand :: TodoCommand -> IO ()
executeShowCommand (Show sa sd f_one f_two) = do
   --print filter_str
   unless (filter_str == "") $ print (getFilters filter_str)
   conn <- connectSqlite3 ".htodo.db"
   getTodoItems conn >>= displayItems
   disconnect conn

   where
      filter_str = concat . intersperse "," . catMaybes $ [f_one, f_two]

displayItems :: [Item] -> IO ()
displayItems = mapM_ (displayItemHelper 0)
   where
      displayItemHelper :: Int -> Item -> IO ()
      displayItemHelper level item = do
         putStr $ take (level * 3 + 1) $ repeat ' '
         putStr $ show (itemId item) ++ ". "
         putStrLn $ itemDescription item
         mapM_ (displayItemHelper (level + 1)) (itemChildren item)
   
getTodoItems :: (IConnection c) => c -> IO [Item]
getTodoItems conn = do
   topLevels <- quickQuery' conn "select i.* from items i where i.parent_id is ?" [SqlNull]
   mapM createChild topLevels
   where
      createChild :: [SqlValue] -> IO Item
      createChild [iid, ide, ica, _, ipr] = do
         let this_id = (fromSql iid) :: Integer
         children <- mapM createChild =<< quickQuery' conn ("select i.* from items i where i.parent_id = " ++ (show this_id)) []
         return Item
                  { itemId = fromSql iid
                  , itemDescription = fromSql ide
                  , itemCreatedAt = fromSql ica
                  , itemPriority = fromSql ipr
                  , itemChildren = children
                  }

data Item = Item
   { itemId :: Integer
   , itemDescription :: String
   , itemCreatedAt :: LocalTime
   , itemPriority :: Integer
   , itemChildren :: [Item]
   }
   deriving(Show, Eq)

executeInitCommand :: TodoCommand -> IO ()
executeInitCommand _ = unimplemented

executeAddCommand :: TodoCommand -> IO ()
executeAddCommand _ = unimplemented

executeEditCommand :: TodoCommand -> IO ()
executeEditCommand _ = unimplemented

executeDoneCommand :: TodoCommand -> IO ()
executeDoneCommand _ = unimplemented

unimplemented = putStrLn "Not Implemented Yet"
