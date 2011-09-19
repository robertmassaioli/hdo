module DataTypes where

import Data.Time (LocalTime)

-- TODO eventually you can remove the deriving(Show) because you will not need to debug it anymore
data List = List
   { listName :: String
   , listMaxIdLen :: Int
   , listItems :: [Item]
   , childLists :: [List]
   } deriving(Show)

data Item = Item
   { itemId :: Integer
   , itemDescription :: String
   , itemCurrentState :: ItemState
   , itemCreatedAt :: LocalTime
   , itemDueDate :: Maybe LocalTime
   , itemPriority :: Integer
   }
   deriving(Show, Eq)

data EventTypes = EventAdd | EventEdit | EventDone | EventRemove
                deriving(Enum, Eq, Show)

data ItemState = StateNotDone | StateDone
               deriving(Enum, Eq, Show)
