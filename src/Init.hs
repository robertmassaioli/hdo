module Init where

import Configuration
import TodoArguments

import System.Directory
import System.FilePath
import Database.HDBC
import Database.HDBC.Sqlite3

createDatabase :: TodoCommand -> Config -> IO ()
createDatabase initCommand config =
   case databaseFile initCommand of
      Nothing -> if userLevel initCommand
                     then runCreateDatabase $ defaultDatabaseLocation config
                     else runCreateDatabase $ defaultDatabaseName config
      Just databasePath -> runCreateDatabase databasePath

runCreateDatabase :: String -> IO ()
runCreateDatabase filename = do
   exist <- doesFileExist filename
   if exist
      then putStrLn $ "Database at '" ++ filename ++ "' already exists. Doing nothing."
      else do
         putStrLn $ "Creating hTodo database: " ++ filename
         conn <- connectSqlite3 filename
         runRaw conn tableUpdates 
         runRaw conn tableLists
         runRaw conn insertMainList
         runRaw conn tableItems
         runRaw conn tableItemEvents
         runRaw conn tableTags
         runRaw conn tableTagMap
         commit conn
         disconnect conn
   
insertMainList = "INSERT INTO lists (id,name,hidden,created_at,parent_id) VALUES (1, 'Main', 0, datetime(), null)"

tableUpdates = "create table updates ( version integer primary key, description text, upgradeDate date );"

tableLists = 
   "create table lists ("
    ++ "id integer primary key autoincrement not null,"
    ++ "name text not null,"
    ++ "hidden INT2 not null,"
    ++ "created_at datetime not null,"
    ++ "parent_id integer,"
    ++ "FOREIGN KEY(parent_id) references lists(id) on delete cascade"
    ++ ");"

tableItems = "create table items ("
              ++ "id integer primary key autoincrement not null,"
              ++ "list_id integer not null,"
              ++ "description text not null,"
              ++ "current_state integer not null default 0,"
              ++ "created_at datetime not null,"
              ++ "priority integer not null,"
              ++ "due_date datetime,"
              ++ "FOREIGN KEY(list_id) references listts(id) on delete cascade"
              ++ ");"

tableItemEvents = "create table item_events ("
                  ++ "id integer primary key autoincrement not null,"
                  ++ "item_id integer not null,"
                  ++ "item_event_type integer not null,"
                  ++ "event_description text,"
                  ++ "occurred_at datetime not null,"
                  ++ "FOREIGN KEY(item_id) references items(id) on delete cascade"
                  ++ ");"

tableTags = "create table tags ("
            ++ "id integer primary key autoincrement not null,"
            ++ "tag_name text not null,"
            ++ "created_at datetime not null"
            ++ ");"

tableTagMap = "create table tag_map ("
            ++ "item_id integer not null,"
            ++ "tag_id integer not null,"
            ++ "created_at datetime not null,"
            ++ "FOREIGN KEY(item_id) references items(id) on delete cascade,"
            ++ "FOREIGN KEY(tag_id) references tags(id) on delete cascade"
            ++ ");"

