{-# OPTIONS_GHC -Wall #-}

module AnalyzeLog where

import Log
import Data.List.Split

-- | Total functions for safety

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x


safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:[]) = Just (x : [])
safeTail (_:xs) = Just xs


safeSecond :: [a] -> Maybe a
safeSecond []       = Nothing
safeSecond (_:y:[]) = Just y
safeSecond (_:y:_)  = Just y
safeSecond _        = Nothing


fhelper :: Maybe String -> String
fhelper (Just x) = x
fhelper  _       = ""


-- Parse an individual message
parseMessage :: String -> LogMessage
parseMessage [] = Unknown "Empty String"
parseMessage (x:xs)
            | x == 'I'  = LogMessage Info (read $ fhelper $ safeHead $ words xs) (restOfMsg $ drop 1 (words xs))
            | x == 'W'  = LogMessage Warning (read $ fhelper $ safeHead $ words xs) (restOfMsg $ drop 1 (words xs))
            | x == 'E'  = LogMessage (Error (read $ fhelper $ safeHead $ words xs))
                                     (read $ fhelper $ safeSecond $ words xs)
                                     (restOfMsg $ drop 2 $ words xs)
            | otherwise = Unknown "Cannot read current message."
           where restOfMsg []        = error "Error"
                 restOfMsg (y:[])    = y ++ []
                 restOfMsg (y:ys)    = y ++ " " ++ restOfMsg ys
                 
                      
-- Split the message
parse :: String -> [LogMessage]
parse x = parseFile $ splitOn "\n" x
    where parseFile []     = error "Empty File"
          parseFile (y:[]) = parseMessage y : []
          parseFile (y:ys) = parseMessage y : parseFile ys
          

-- Insert a LogMessage into a tree
insert :: LogMessage -> MessageTree -> MessageTree
insert logMsg Leaf                              = Node Leaf logMsg Leaf
insert logMsg (Node a (LogMessage ba bb bc ) c) = case logMsg of
              Unknown    _                     -> Node a (LogMessage ba bb bc) c
              LogMessage _ x _
                         | x < bb              -> Node Leaf logMsg (Node a (LogMessage ba bb bc) c)
                         | x > bb              -> Node (Node a (LogMessage ba bb bc) c) logMsg Leaf
              LogMessage   _ _ _               -> error "Could not sort message into list."
insert _      _                                 = error "Could not sort message into list."


-- Build a tree to parse the logs
build :: [LogMessage] -> MessageTree
build [] = Leaf
build x  = buildMessageTree x Leaf
     where buildMessageTree []     _    = Leaf
           buildMessageTree (y:[]) tree = insert y tree
           buildMessageTree (y:ys) tree = buildMessageTree ys (insert y tree)


-- Sort the tree 
inOrder :: MessageTree -> [LogMessage]
inOrder tree = case tree of
       Node a msg Leaf -> msg : inOrder a 
       Node Leaf msg b -> msg : inOrder b
       Node _    msg _ -> msg : []
       Leaf            -> error "Couldn't create log of messages"


-- Filter for all of the errors
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong y =
  let getErrors []                              str = str
      getErrors ((LogMessage (Error x) _ c):[]) str = if x > 50
                                                      then c : str
                                                      else str
      getErrors ((LogMessage  _ _ _)       :[]) str = str
      getErrors ((LogMessage (Error x) _ c):xs) str
                                  | x > 50          = getErrors xs (c : str)
                                  | otherwise       = getErrors xs str

      getErrors ((LogMessage _ _ _)        :xs) str = getErrors xs str
      getErrors (_:xs)                          str = getErrors xs str
      
  in  getErrors y []
