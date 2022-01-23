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
