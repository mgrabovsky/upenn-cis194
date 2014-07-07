{-# OPTIONS_GHC -Wall #-}

module LogAnalysis ( module Log
                   , parseMessage
                   , parse
                   , insert
                   , build
                   , inOrder
                   , whatWentWrong ) where

import Log

parseMessage :: String -> LogMessage
parseMessage line = case words line of
                        ("I":ts:msg) -> correct Info ts msg
                        ("W":ts:msg) -> correct Warning ts msg
                        ("E":code:ts:msg) -> correct (Error $ read code) ts msg
                        _ -> Unknown line
                    where correct mtype ts msg = LogMessage mtype (read ts) (unwords msg)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert entry@(LogMessage _ ts _) tree
    | Leaf <- tree
    = Node Leaf entry Leaf
    | Node left root@(LogMessage _ ts' _) right <- tree
    = if ts > ts' then (Node left root (insert entry right))
                  else (Node (insert entry left) root right)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (m:ms) = insert m (build ms)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left entry right) = inOrder left ++ [entry] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map message . inOrder . build . filter relevant
    where relevant (LogMessage (Error severity) _ _) = severity >= 50
          relevant _ = False
          message (LogMessage _ _ msg) = msg

