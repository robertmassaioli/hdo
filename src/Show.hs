module Show
   ( executeShowCommand
   ) where

import Control.Monad
import Data.List
import Database.HDBC
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord(comparing)

import Util
import DataTypes
import TodoArguments
import Configuration
import Filter

-- TODO use the MaybeT transformer
executeShowCommand :: Config -> TodoCommand -> IO ()
executeShowCommand config showFlags = do
   mconn <- getDatabaseConnection config showFlags
   case mconn of
      Nothing -> gracefulExit
      Just conn -> do
         -- TODO Actually use these filter strings.
         --unless (filter_str == "") $ print (getFilters filter_str)
         case showUsingTags showFlags of
            Nothing -> getTodoLists showFlags conn >>= sequence_ . intersperse putNewline . fmap (displayList 0)
            Just x -> case separateCommas x of
                        Nothing -> putStrLn "Invalid text was placed in the tags."
                        Just x -> print "Not implemented yet"
         disconnect conn
   where
      filter_str = intercalate "," . catMaybes $ [showUsingFilter showFlags, showFilterExtra showFlags]

getTodoLists :: (IConnection c) => TodoCommand -> c -> IO [List]
getTodoLists showFlags conn = do
   topLevels <- quickQuery' conn "SELECT l.* FROM lists l where l.parent_id is null order by l.name, l.created_at" []
   mapM createChildList topLevels
   where
      whichState = if showDone showFlags then StateDone else StateNotDone

      createChildList :: [SqlValue] -> IO List
      createChildList [lid, lname, lhidden, lcreatedAt, lparentId] = do
         children <- mapM createChildList =<< quickQuery' conn "SELECT l.* FROM lists l WHERE l.parent_id = ? order by l.name, l.created_at" [lid]
         maxItemId <- fmap (fromMaybe 1 . extractInteger) $ quickQuery' conn "SELECT max(i.id) FROM items i, lists l WHERE ? = l.id AND l.id = i.list_id" [lid]
         items <- mapM toItem =<< quickQuery' conn "SELECT i.* FROM items i, lists l WHERE ? = l.id AND l.id = i.list_id AND i.current_state <= ? ORDER BY i.priority, i.id" [lid, toSql . fromEnum $ whichState]
         return List 
            { listName = fromSql lname
            , listMaxIdLen = fromInteger maxItemId
            , listItems = items
            , childLists = children
            }
         where
            toItem :: [SqlValue] -> IO Item
            toItem [iId, iListId, iDescription, iCurrentState, iCreatedAt, iPriority, iDueDate] = 
               return Item 
                  { itemId = fromSql iId
                  , itemDescription = fromSql iDescription
                  , itemCurrentState = toEnum . fromSql $ iCurrentState
                  , itemCreatedAt = fromSql iCreatedAt
                  , itemDueDate = fromSql iDueDate
                  , itemPriority = fromSql iPriority
                  }

hasItems :: List -> Bool
hasItems currentList = haveItems || childrenHaveItems
   where
      haveItems = not . null . listItems $ currentList
      childrenHaveItems = any hasItems (childLists currentList)

displayList :: Int -> List -> IO ()
displayList indentLevel list = when (hasItems list) $ do
   putStr indentSpace
   putStrLn $ listName list ++ ":"
   unless noItems $ do
      mapM_ displayItem (listItems list)
      unless noChildren putNewline
   unless noChildren $
      sequence_ . intersperse putNewline . fmap (displayList $ indentLevel + 1) $ theChildren
   where
      noItems = null . listItems $ list
      noChildren = null theChildren

      itemIndentSpace = replicate (spacesPerIndent * (indentLevel + 1)) ' '
      indentSpace = replicate (spacesPerIndent * indentLevel) ' '
      spacesPerIndent = 3

      theChildren = childLists list

      displayItem :: Item -> IO ()
      displayItem item = do
         putStr $ itemIndentSpace
         case itemCurrentState item of
            StateDone   -> putStr "-"
            _           -> return ()
         putStr $ itemIdString ++ "." ++ extraSpaces
         putStrLn $ itemDescription item
         where
            extraSpaces :: String
            extraSpaces = replicate (1 + (length . show . listMaxIdLen $ list) - length itemIdString) ' '

            itemIdString = show $ itemId item
