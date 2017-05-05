{-# OPTIONS_GHC -Wall #-}

module AnalyzeLog where

import Log
import Data.Bool
import Data.List.Split

-- | Safe helper Functions

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

readFirst :: Read a => String -> a
readFirst = read . fhelper . safeHead . words

readSecond :: Read a => String -> a
readSecond = read . fhelper . safeSecond . words

fdrop :: ([String] -> String) -> Int -> String -> String
fdrop f n = f . drop n . words

-- | Other Helper functions

-- Parse an individual message
parseMessage :: String -> LogMessage
parseMessage [] = Unknown "Empty String"
parseMessage (x:xs)
            | x == 'I'               = LogMessage Info (readFirst xs) (fdrop restOfMsg 1 xs)
            | x == 'W'               = LogMessage Warning (readFirst xs) (fdrop restOfMsg 1 xs)
            | x == 'E'               = LogMessage (Error (readFirst xs)) (readSecond xs) (fdrop restOfMsg 2 xs)
           where restOfMsg []        = []
                 restOfMsg (y:[])    = y ++ []
                 restOfMsg (y:ys)    = y ++ " " ++ restOfMsg ys
parseMessage _                       = Unknown "Cannot read current message."

-- Split the message into the log type
parse :: String -> [LogMessage]
parse x = parseFile $ splitOn "\n" x
    where parseFile []     = [Unknown "Empty File"]
          parseFile (y:[]) = parseMessage y : []
          parseFile (y:ys) = parseMessage y : parseFile ys


-- Insert a LogMessage into a binary tree
insert :: LogMessage -> MessageTree -> MessageTree
insert logMsg Leaf                              = Node Leaf logMsg Leaf
insert logMsg (Node a (LogMessage ba bb bc ) c) = case logMsg of
              Unknown    _                     -> Node a (LogMessage ba bb bc) c
              LogMessage _ x _
                         | x < bb              -> Node Leaf logMsg (Node a (LogMessage ba bb bc) c)
                         | x > bb              -> Node (Node a (LogMessage ba bb bc) c) logMsg Leaf
              _                                -> error "Could not sort message into list."
insert _      _                                 = error "Could not sort message into list."


-- Build a binary tree out of the log messages
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
  let getErrors []                              str              = str
      getErrors ((LogMessage (Error x) _ c):[]) str              = bool str (c : str) (x > 50)
      getErrors ((LogMessage  _ _ _)       :[]) str              = str
      getErrors ((LogMessage (Error x) _ c):xs) str | x > 50     = getErrors xs (c : str)
                                                    | otherwise  = getErrors xs str
      getErrors ((LogMessage _ _ _)        :xs) str              = getErrors xs str
      getErrors (_:xs)                          str              = getErrors xs str

  in  getErrors y []