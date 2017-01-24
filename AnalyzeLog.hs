-- Analyze Server Logs
-- Copyright (c) 2017 Robert Stefanic
--
-- MIT License

{-# OPTIONS_GHC -Wall #-}

module AnalyzeLog where

import Log
import Data.List.Split

parseMessage :: String -> LogMessage
parseMessage [] = Unknown "Empty String"
parseMessage (x:xs)
            | x == 'I'  = LogMessage Info (read $ head $ words xs) (restOfMsg $ drop 1 (words xs))
            | x == 'W'  = LogMessage Warning (read $ head $ words xs) (restOfMsg $ drop 1 (words xs))
            | x == 'E'  = LogMessage (Error (read $ head $ words xs))
                                     (read $ head $ tail $ words xs)
                                     (restOfMsg $ drop 2 $ words xs)
            | otherwise = Unknown "Cannot read current message."
           where restOfMsg []        = error "Error"
                 restOfMsg (y:[])    = y ++ []
                 restOfMsg (y:ys)    = y ++ " " ++ restOfMsg ys
                 

parse :: String -> [LogMessage]
parse x = parseFile $ splitOn "\n" x
    where parseFile []     = error "Empty File"
          parseFile (y:[]) = parseMessage y : []
          parseFile (y:ys) = parseMessage y : parseFile ys


insert :: LogMessage -> MessageTree -> MessageTree
insert logMsg Leaf                              = Node Leaf logMsg Leaf
insert logMsg (Node a (LogMessage ba bb bc ) c) = case logMsg of
              Unknown    _                     -> Node a (LogMessage ba bb bc) c
              LogMessage _ x _
                         | x < bb              -> Node Leaf logMsg (Node a (LogMessage ba bb bc) c)
                         | x > bb              -> Node (Node a (LogMessage ba bb bc) c) logMsg Leaf
              LogMessage   _ _ _               -> error "Could not sort message into list."
insert _      _                                 = error "Could not sort message into list."


build :: [LogMessage] -> MessageTree
build [] = Leaf
build x  = buildMessageTree x Leaf
     where buildMessageTree []     _    = Leaf
           buildMessageTree (y:[]) tree = insert y tree
           buildMessageTree (y:ys) tree = buildMessageTree ys (insert y tree)
  

inOrder :: MessageTree -> [LogMessage]
inOrder tree = case tree of
       Node a msg Leaf -> msg : inOrder a 
       Node Leaf msg b -> msg : inOrder b
       Node _    msg _ -> msg : []
       Leaf            -> error "Couldn't create log of messages"


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
