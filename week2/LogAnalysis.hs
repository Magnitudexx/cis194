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

insert :: LogMessage -> MessageTree -> MessageTree

-- handle Unknown messages 
insert (Unknown _) tree = tree
insert LogMessage {} tree@(Node _ (Unknown _) _) = tree

--If tree is Leaf return Node
insert logMsg Leaf = Node Leaf logMsg Leaf

-- Traverse tree
insert p@(LogMessage _ ts _) (Node left nodeMsg@(LogMessage _ nodeTs _) right) 
    | ts > nodeTs   = Node left nodeMsg (insert p right)
    | otherwise     = Node (insert p left) nodeMsg right

