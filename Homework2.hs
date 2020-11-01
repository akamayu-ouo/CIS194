{-# OPTIONS_GHC -Wall #-}
{-- CIS 194: Homework2 --}
{-- https://www.seas.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf --}
module LogAnalysis where

import Log


-- Exercise 1:
isNumber :: String -> Bool
isNumber [] = False
isNumber (x:xs) = (inDigit ['1'..'9'] x) && all (inDigit ['0'..'9']) xs
    where inDigit = flip elem

parseMessage :: String -> LogMessage
parseMessage = checkTime . checkType . words
    where checkType ("E" : lv : msg) =
            if isNumber lv
                then ((Just (Error $ read lv)), msg)
                else (Nothing, lv : msg)
          checkType ("I" : msg) = ((Just Info), msg)
          checkType ("W" : msg) = ((Just Warning), msg)
          checkType msg = (Nothing, msg)
          checkTime (Just tp, (ts : msg)) =
            if isNumber ts
                then LogMessage tp (read ts) (unwords msg)
                else Unknown (unwords msg)
          checkTime (_, msg) = Unknown (unwords msg)

parse :: String -> [LogMessage]
parse = map parseMessage . lines


-- Exercise 2:
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg1 (Node tree1 msg2 tree2) =
    if (getTime msg1) < (getTime msg2)
        then (Node (insert msg1 tree1) msg2 tree2)
        else (Node tree1 msg2 (insert msg1 tree2))
            where getTime (LogMessage _ time _ ) = time
                  getTime (Unknown _) = error "No Unknown should appear in a MessageTree"


-- Exercise 3:
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


-- Exercise 4:
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node t1 value t2) = (inOrder t1) ++ [value] ++ (inOrder t2)


-- Exercise 5:
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . inOrder . build . filter (severity 50)
                    where getMessage (LogMessage _ _ msg) = msg
                          getMessage (Unknown msg) = msg
                          severity threshold (LogMessage (Error lv) _ _) = lv > threshold
                          severity _ _ = False
