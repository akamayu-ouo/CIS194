{-- CIS 194: Homework7 --}
{-- https://www.seas.upenn.edu/~cis194/spring13/hw/07-folds-monoids.pdf --}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module JoinList where

import Data.List (foldl')

import Editor 
import Buffer
import Scrabble
import Sized

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


-- Exercise 1:
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty Empty = Empty
(+++) x y         = Append (tag x <> tag y) x y

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m


-- Exercise 2:
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n (Append _ l r)
    | n < sz    =  indexJ n l
    | otherwise =  indexJ (n - sz) r
    where sz = getSize . size . tag $ l
indexJ n (Single _ a)  
    | n == 0    = Just a
indexJ _ _  = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n (Append _ l r)
    | n < sz    = dropJ n l +++ r
    | otherwise = dropJ (n - sz) r
    where sz = getSize . size . tag $ l
dropJ n x@(Single _ _)
    | n <= 0    = x
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n (Append _ l r)
    | n < sz    = takeJ n l
    | otherwise = l +++ takeJ (n - sz) r
    where sz = getSize . size . tag $ l
takeJ n x@(Single _ _)
    | n >= 1 = x
takeJ _ _ = Empty


-- Exercise 3:
instance Buffer (JoinList (Score, Size) String) where
    toString       = unlines . jlToList
    fromString     = foldl' (\x y -> x +++ singleMaker y) Empty . lines
        where singleMaker s = Single (scoreString s, 1) s
    line           = indexJ
    replaceLine n l b 
        | sz > n && n >= 0 = takeJ n b +++ fromString l +++ dropJ (n+1) b
        | otherwise        = b
        where sz = numLines b
    numLines       = getSize . size . snd . tag
    value          = getSize . size . fst . tag

text = unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
buff = fromString text :: JoinList (Score, Size) String
test = runEditor editor buff
