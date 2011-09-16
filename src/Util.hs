module Util where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Database.HDBC

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Show.Pretty

prettyShow :: (Show a) => a -> IO ()
prettyShow = putStrLn . ppShow

putNewline :: IO ()
putNewline = putStrLn ""

separateBy :: Char -> String -> Maybe [String]
separateBy sep input = 
   case parse parseCommas "(unknown)" input of
      Left _ -> Nothing
      Right x -> Just x
   where
      parseCommas :: Parser [String]
      parseCommas = sepBy (many1 (noneOf [sep])) (char sep)

separateCommas :: String -> Maybe [String]
separateCommas = separateBy ','

-- Use this when you error out and you knew it was possible that the error would
-- occur. Use it to let the user know that this is expected behaviour considering what the program
-- could find.
gracefulExit :: IO ()
gracefulExit = putStrLn "hTodo shutdown gracefully."

getLastId :: (IConnection c) => c -> IO Integer
getLastId conn = fmap extractIntegerOrDie $ quickQuery' conn "select last_insert_rowid()" []
   where
      extractIntegerOrDie :: [[SqlValue]] -> Integer
      extractIntegerOrDie [[x]] = fromSql x
      extractIntegerOrDie _     = error "I expected to be able to extract an integer and that did not happen."

extractInteger :: [[SqlValue]] -> Maybe Integer
extractInteger [[SqlNull]] = Nothing
extractInteger [[x]] = Just $ fromSql x
extractInteger _     = Nothing

findOrCreateTags :: (IConnection c) => c -> Integer -> [String] -> IO [Integer]
findOrCreateTags conn itemId = mapM findOrCreateTag
   where
      findOrCreateTag :: String -> IO Integer
      findOrCreateTag tag = do
         res <- fmap extractInteger $ quickQuery' conn "select id from tags where tag_name = ?" [toSql tag]
         case res of
            Just x -> return x
            Nothing -> do
               run conn "INSERT INTO tags (tag_name, created_at) VALUES (?, datetime())" [toSql tag]
               getLastId conn

-- TODO I think that I need to create a database specific Util file.
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

-- TODO this function only handles the top level list id but there may be more to it, more
-- levels deep
getOrCreateListId :: (IConnection c) => c -> Maybe String -> IO Integer
getOrCreateListId _     Nothing     = return 1 
getOrCreateListId _     (Just [])   = return 1
getOrCreateListId conn  (Just xs)   = go Nothing (fromMaybe [] $ separateBy '/' xs)
   where 
      go :: Maybe Integer -> [String] -> IO Integer
      go Nothing []        = return 1
      go (Just listId) []  = return listId
      go listId (name:xs)  = do
         result <- fmap extractInteger $ quickQuery' conn (selectQuery listId) [toSql name, toSql listId]
         case result of 
            Nothing -> do
               run conn "INSERT INTO lists(name, hidden, created_at, parent_id) VALUES (?, 0, datetime(), ?)" [toSql name, toSql listId]
               lastId <- getLastId conn
               go (Just lastId) xs
            existing -> go existing xs
         where
            selectQuery :: Maybe Integer -> String
            selectQuery Nothing  = baseSelectQuery ++ " is ?"
            selectQuery (Just _) = baseSelectQuery ++ " = ?"
            
            baseSelectQuery = "SELECT id FROM lists WHERE name = ? AND parent_id"

getListId :: IConnection c => c -> String -> IO (Maybe Integer)
getListId _    [] = return . Just $ 1
getListId conn xs = go Nothing (fromMaybe [] $ separateBy '/' xs)
   where
      go :: Maybe Integer -> [String] -> IO (Maybe Integer)
      go Nothing [] = return . Just $ 1
      go ret@(Just _) [] = return ret
      go listId (name:names) = do
         result <- fmap extractInteger $ quickQuery' conn (selectQuery listId) [toSql name, toSql listId]
         case result of
            Nothing -> return Nothing
            existing -> go existing names
         where 
            selectQuery :: Maybe Integer -> String
            selectQuery Nothing = baseSelectQuery ++ " is ?"
            selectQuery (Just _) = baseSelectQuery ++ " = ?"

            baseSelectQuery = "SELECT id FROM lists WHERE name = ? and parent_id"
