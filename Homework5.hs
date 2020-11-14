{-- CIS 194: Homework5 --}
{-- https://www.seas.upenn.edu/~cis194/spring13/hw/05-type-classes.pdf --}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Calc where
import ExprT
import Parser (parseExp)
import qualified StackVM as VM
import qualified Data.Map as M


--  Exercise1
eval :: ExprT -> Integer
eval (ExprT.Lit x  ) = x
eval (ExprT.Add a b) = (eval a) + (eval b)
eval (ExprT.Mul a b) = (eval a) * (eval b)


--  Exercise2
evalStr :: String -> Maybe Integer
evalStr x = evalStr' $ parseExp ExprT.Lit ExprT.Add ExprT.Mul x
    where evalStr' Nothing  = Nothing
          evalStr' (Just a) = Just (eval a)


--  Exercise3
class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    mul = ExprT.Mul
    add = ExprT.Add


--  Exercise4
instance Expr Integer where
    lit = id
    mul = (*)
    add = (+)

instance Expr Bool where
    lit = (>0)
    mul = (||)
    add = (&&)

newtype MinMax = MinMax Integer
    deriving (Eq, Show)
instance Expr MinMax where
    lit = MinMax
    mul (MinMax a) (MinMax b) = MinMax $ max a b
    add (MinMax a) (MinMax b) = MinMax $ min a b

newtype Mod7 = Mod7 Integer
    deriving (Eq, Show)
instance Expr Mod7 where
    lit = mod7Maker
    mul (Mod7 a) (Mod7 b) = mod7Maker $ (*) a b
    add (Mod7 a) (Mod7 b) = mod7Maker $ (+) a b

mod7Maker :: Integer -> Mod7
mod7Maker x = Mod7 $ x `mod` 7


-- Exercise5
instance Expr VM.Program where
    lit x = [VM.PushI x]
    add a b = a ++ b ++ [VM.Add]
    mul a b = a ++ b ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul


-- Exercise6
class HasVars a where
    var :: String -> a

-- Augment `ExprT` with a `Var` constructor to represent a variable
data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

instance HasVars VarExprT where
    var = Var

instance Expr VarExprT where
    lit = Calc.Lit
    mul = Calc.Mul
    add = Calc.Add

-- Use function to represent an expression
instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    --Numeral literial is just a constant function
    lit = const . Just -- \_ -> Just x
    -- <$> applies a function on the content of a monad
    -- <*> applies a function in monad on the content of a monad
    add f g = \m -> (+) <$> (f m) <*> (g m)
    mul f g = \m -> (*) <$> (f m) <*> (g m)
