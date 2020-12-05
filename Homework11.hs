{-- CIS 194: Homework10 --}
{-- https://www.cis.upenn.edu/~cis194/spring13/hw/11-applicative2.pdf --}
{-# OPTIONS_GHC -Wall #-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []


oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (++) <$> oneOrMore  (satisfy isAlpha)
             <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show


parseSExpr :: Parser SExpr
parseSExpr = spaces *> (A <$> parseAtom <|> Comb <$> parseComb) <* spaces
      where parseAtom  = N <$> posInt <|> I <$> ident
            parseComb  = char '(' *> oneOrMore parseSExpr <* char ')'
      -- a *> b discard the first result
      -- a <* b discard the second result
