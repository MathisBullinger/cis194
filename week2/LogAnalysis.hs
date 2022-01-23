{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage

-- this doesn't seem very DRY...
parseMessage str = let parts = words str in
                   case parts of
                     ("I":ts:msg)     -> LogMessage Info (read ts) (unwords msg)
                     ("W":ts:msg)     -> LogMessage Warning (read ts) (unwords msg)
                     ("E":lvl:ts:msg) -> LogMessage (Error $ read lvl) (read ts) (unwords msg)
                     _                -> Unknown str

parse :: String -> [LogMessage]
parse str = map parseMessage (lines str)

insert :: LogMessage -> MessageTree -> MessageTree

insert (Unknown _) tree = tree
insert msg Leaf         = Node Leaf msg Leaf
insert msg (Node l m g) = let (LogMessage _ ts _) = m
                              (LogMessage _ tsI _) = msg in 
                          if tsI < ts
                            then Node (insert msg l) m g 
                            else Node l m (insert msg g)

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l m g) = (inOrder l) ++ [m] ++ (inOrder g)
