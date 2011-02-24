module Show
   ( executeShowCommand
   ) where

import Control.Monad
import Data.List
import Database.HDBC
import Data.Maybe (catMaybes)
import Data.Ord(comparing)

import Util
import DataTypes
import TodoArguments
import Configuration
import Filter

executeShowCommand :: Config -> TodoCommand -> IO ()
executeShowCommand config showFlags = do
   mconn <- getDatabaseConnection config showFlags
   case mconn of
      Nothing -> gracefulExit
      Just conn -> do
         unless (filter_str == "") $ print (getFilters filter_str)
         case showUsingTags showFlags of
            --Nothing -> getTodoItems conn (generateQuery []) >>= displayItems maxId
            Nothing -> getTodoLists conn >>= sequence_ . intersperse putNewline . fmap (displayList 0)
            Just x -> case separateCommas x of
                        Nothing -> putStrLn "Invalid text was placed in the tags."
                        Just x -> print "Not implemented yet"
         disconnect conn
   where
      filter_str = intercalate "," . catMaybes $ [showUsingFilter showFlags, showFilterExtra showFlags]

      generateQuery :: [String] -> String
      generateQuery [] = "SELECT i.* FROM items i where i.current_state < ? "
      generateQuery xs = queryLeft ++ " AND (" ++ createOrList "t.tag_name =" xs ++ ")"

      queryLeft = "SELECT i.* FROM items i, tags t, tag_map tm where i.id = tm.item_id AND tm.tag_id = t.id AND i.current_state < ?"

data List = List
   { listName :: String
   , listMaxIdLen :: Int
   , listItems :: [Item]
   , childLists :: [List]
   } deriving(Show)

getTodoLists :: (IConnection c) => c -> IO [List]
getTodoLists conn = do
   topLevels <- quickQuery' conn "SELECT l.* FROM lists l where l.parent_id is null order by l.name, l.created_at" []
   mapM createChildList topLevels
   where
      createChildList :: [SqlValue] -> IO List
      createChildList [lid, lname, lhidden, lcreatedAt, lparentId] = do
         children <- mapM createChildList =<< quickQuery' conn "SELECT l.* FROM lists l WHERE l.parent_id = ? order by l.name, l.created_at" [lid]
         maxItemId <- fmap (maybe 1 id . extractInteger) $ quickQuery' conn "SELECT max(i.id) FROM items i, lists l WHERE ? = l.id AND l.id = i.list_id" [lid]
         items <- mapM toItem =<< quickQuery' conn "SELECT i.* FROM items i, lists l WHERE ? = l.id AND l.id = i.list_id ORDER BY i.priority, i.id" [lid]
         return $ List 
            { listName = fromSql lname
            , listMaxIdLen = fromInteger maxItemId
            , listItems = items
            , childLists = children
            }
         where
            toItem :: [SqlValue] -> IO Item
            toItem [iId, iListId, iDescription, iCurrentState, iCreatedAt, iPriority, iDueDate] = 
               return $ Item 
                  { itemId = fromSql iId
                  , itemDescription = fromSql iDescription
                  , itemCreatedAt = fromSql iCreatedAt
                  , itemDueDate = fromSql iDueDate
                  , itemPriority = fromSql iPriority
                  }

displayList :: Int -> List -> IO ()
displayList indentLevel list = do
   putStr indentSpace
   putStrLn $ listName list ++ ":"
   mapM_ displayItem $ listItems list
   unless (null . childLists $ list) $ do
      putNewline
      sequence_ . intersperse putNewline $ fmap (displayList $ indentLevel + 1) (childLists list)
   where
      indentSpace = replicate (spacesPerIndent * indentLevel) ' '
      spacesPerIndent = 3

      displayItem :: Item -> IO ()
      displayItem item = do
         putStr $ indentSpace ++ replicate spacesPerIndent ' ' ++ itemIdString ++ "." ++ extraSpaces
         putStrLn $ itemDescription item
         where
            extraSpaces :: String
            extraSpaces = replicate (1 + (length . show . listMaxIdLen $ list) - length itemIdString) ' '

            itemIdString = show $ itemId item

createListType :: (Show a) => String -> String -> [a] -> String
createListType comb prefix values = 
   intercalate (" " ++ comb ++ " ") $ zipWith joinFunc (repeat prefix) (fmap show values)
   where 
      joinFunc a b = a ++ " " ++ b

surround :: a -> [a] -> [a]
surround a xs = [a] ++ xs ++ [a]

createOrList :: (Show a) => String -> [a] -> String
createOrList = createListType "OR"

createAndList :: (Show a) => String -> [a] -> String
createAndList = createListType "AND"
