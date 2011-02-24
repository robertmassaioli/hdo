module DataTypes where

import Data.Time (LocalTime)

data Item = Item
   { itemId :: Integer
   , itemDescription :: String
   , itemCreatedAt :: LocalTime
   , itemDueDate :: Maybe LocalTime
   , itemPriority :: Integer
   }
   deriving(Show, Eq)

data EventTypes = EventAdd | EventEdit | EventDone | EventRemove
                deriving(Enum, Eq, Show)

data ItemState = StateNotDone | StateDone
               deriving(Enum, Eq, Show)
