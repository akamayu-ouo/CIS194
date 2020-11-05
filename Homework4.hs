{-# OPTIONS_GHC -Wall #-}
{-- CIS 194: Homework4 --}
{-- https://www.seas.upenn.edu/~cis194/spring13/hw/04-higher-order.pdf --}
import Data.List ((\\), foldl')


--Exercise 1
fun1' :: [Integer] -> Integer
fun1' = foldl' ((*).(subtract 2)) 1 . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate nxt
  where nxt n = if even n then (n `div` 2) else (3 * n + 1)


--Exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf

insertNode :: a -> Tree a -> Tree a
insertNode x Leaf = Node 0 Leaf x Leaf
insertNode x (Node h lt v rt) 
  | lh < rh   = Node h  rt v lt'
  | lh > rh   = Node h  lt v rt'
  | otherwise = Node h' rt v lt'
  where lh  = height lt
        rh  = height rt
        h'  = 1 + height lt'
        lt' = insertNode x lt
        rt' = insertNode x rt

height :: Tree a -> Integer
height Leaf            = -1
height (Node h _ _ _ ) = h

{--
isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node h lt _ rt) = (1 >= abs (lh - rh)) && isBalanced lt && isBalanced rt
  where lh = height lt
        rh = height rt
--}


--Exercise 3
xor :: [Bool] -> Bool
xor = foldl' (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:).f) []


--Exercise 4
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base $ reverse xs


--Exercise 5
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1).(*2)) $ [1..n] \\ bye
  where bye    = filter (<=n) [b x y | x <- [1..n], y <- [x..n]]
        b i j  = 2 * i * j + i + j
