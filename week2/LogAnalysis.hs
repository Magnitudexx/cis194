{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage str = case words str of
    ("I":ts:rest)   -> LogMessage Info (read ts :: Int) (unwords rest)
    ("W":ts:rest)   -> LogMessage Warning (read ts :: Int) (unwords rest)
    ("E":n:ts:rest) -> LogMessage (Error (read n :: Int)) (read ts :: Int) (unwords rest)
    _ -> Unknown str

parse :: String -> [LogMessage]
parse str = case lines str of 
    []          -> []
    (ln:rest)   -> parseMessage ln : parse (unlines rest) 
