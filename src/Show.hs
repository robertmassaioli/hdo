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
         maxId <- fmap getSingleValue $ quickQuery' conn "select max(id) from items;" []
         case showUsingTags showFlags of
            Nothing -> getTodoItems conn (generateQuery []) >>= displayItems maxId
            Just x -> case separateCommas x of
                        Nothing -> putStrLn "Invalid text was placed in the tags."
                        Just x -> getTodoItems conn (generateQuery x) >>= displayItems maxId
         disconnect conn
   where
      filter_str = intercalate "," . catMaybes $ [showUsingFilter showFlags, showFilterExtra showFlags]

      generateQuery :: [String] -> String
      generateQuery [] = "SELECT i.* FROM items i where i.current_state < ? "
      generateQuery xs = queryLeft ++ " AND (" ++ createOrList "t.tag_name =" xs ++ ")"

      getSingleValue :: [[SqlValue]] -> Int
      getSingleValue [[x]] = length . show $ getInt 
         where getInt :: Int
               getInt = fromSql x
      getSingleValue _ = 1

      queryLeft = "SELECT i.* FROM items i, tags t, tag_map tm where i.id = tm.item_id AND tm.tag_id = t.id AND i.current_state < ?"

displayItems :: Int -> [Item] -> IO ()
displayItems maxLen = mapM_ (displayItemHelper 0)
   where
      displayItemHelper :: Int -> Item -> IO ()
      displayItemHelper level item = do
         putStr $ replicate (level * 3 + 1) ' '
         putStr $ show (itemId item) ++ "." ++ spacesLen
         putStrLn $ itemDescription item
            where
               itemString = show (itemId item)
               spacesLen = take (1 + maxLen - (length itemString)) $ repeat ' '

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
   
getTodoItems :: (IConnection c) => c -> String -> IO [Item]
getTodoItems conn baseQuery = do
   topLevels <- quickQuery' conn topLevelQuery [toSql $ fromEnum StateDone, SqlNull]
   fmap sortItems $ mapM createChild topLevels
   where
      topLevelQuery = baseQuery ++ " AND i.parent_id is ?"
      childQuery = baseQuery ++ " AND i.parent_id = ?"

      createChild :: [SqlValue] -> IO Item
      createChild [iId, iDescription, iStatus, iCreatedAt, _, iParent] = do
         let this_id = fromSql iId :: Integer
         children <- mapM createChild =<< quickQuery' conn childQuery [toSql $ fromEnum StateDone, toSql this_id]
         return Item
                  { itemId = fromSql iId
                  , itemDescription = fromSql iDescription
                  , itemCreatedAt = fromSql iCreatedAt
                  , itemPriority = fromSql iParent
                  }

      sortItems :: [Item] -> [Item]
      sortItems = sortBy $ \x y -> comparing itemPriority x y
