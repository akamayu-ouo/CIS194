{-- CIS 194: Homework7 --}
{-- https://www.seas.upenn.edu/~cis194/spring13/hw/07-folds-monoids.pdf --}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import Data.Char (toUpper)
import Data.List (foldl')

import Sized

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Sized Score where
    size (Score i) = Size i

instance Monoid Score where
    mempty  = Score 0
    mappend = (+)

instance Semigroup Score where
    x <> y  = x `mappend` y

score :: Char -> Score
score = Score . table . toUpper
    where table c
            | c `elem` "AEILNORSTU" = 1
            | c `elem` "DG"         = 2
            | c `elem` "BCMP"       = 3
            | c `elem` "FHVWY"      = 4
            | c `elem` "K"          = 5
            | c `elem` "JX"         = 8
            | c `elem` "QZ"         = 10
            | otherwise             = 0

scoreString :: String -> Score
scoreString = foldl' (<>) mempty . map score
