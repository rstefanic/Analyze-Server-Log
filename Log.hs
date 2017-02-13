module Log where

import Control.Applicative

-- | Data Types

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)


-- | Read the error log and construct a binary tree out of the log
parseErrors :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
parseErrors parse n file = take n . parse <$> readFile file

-- | Read the error log and find only the errors
reconstructErrors :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
reconstructErrors parse whatWentWrong file
  = whatWentWrong . parse <$> readFile file
