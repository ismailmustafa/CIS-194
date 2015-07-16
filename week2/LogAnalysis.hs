{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- CIS 194: Homework 2

-- Exercise 1
--
-- Parse the entire log file
parse :: String -> [LogMessage]
parse = map parseMessage . lines 

-- Parses an individual line from the log file
parseMessage :: String -> LogMessage
parseMessage str
    | checkType == "I" = LogMessage Info (wordsAt 1) 
                                         (restOfString 2)
    | checkType == "E" = LogMessage (Error $ wordsAt 1) 
                                    (wordsAt 2) 
                                    (restOfString 3)
    | checkType == "W" = LogMessage Warning (wordsAt 1) 
                                            (restOfString 2)
    | otherwise        = Unknown str
        where
            checkType      = head $ words str
            wordsAt n      = read $ (words str) !! n
            restOfString n = unwords $ drop n $ words str

-- Exercise 2
--
-- Insert new log message into existing message tree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert lx@(LogMessage _ tx _) (Node l ly@(LogMessage _ ty _) r)
    | tx <= ty          = Node (insert lx l) ly r
    | tx > ty           = Node l ly (insert lx r)
insert lx _             = Node Leaf lx Leaf

-- Exercise 3
--
-- Build up message tree from list of log messages
build :: [LogMessage] -> MessageTree
build = buildHelper Leaf

buildHelper :: MessageTree -> [LogMessage] -> MessageTree
buildHelper tree (x:xs) = buildHelper (insert x tree) xs
buildHelper tree _      = tree

-- Exercise 4
--
-- Sort log messages by time stamps using in order traversal
inOrder :: MessageTree -> [LogMessage]
inOrder (Node l lm r) = (inOrder l)++[lm]++(inOrder r)
inOrder Leaf          = []

-- Exercise 5
--
-- extract messages with severity greater than 50
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = [s | (LogMessage _ _ s) <- orderedErrors]
    where
        errorsOnly    = [x | x@(LogMessage (Error s) _ _) <- xs, s >= 50]
        orderedErrors = inOrder $ build errorsOnly
