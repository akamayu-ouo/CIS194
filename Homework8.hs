{-- CIS 194: Homework8 --}
{-- https://www.seas.upenn.edu/~cis194/spring13/hw/08-IO.pdf --}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee (Employee(..), GuestList(..))
import Data.Tree (Tree(..))
import Data.List (sort)


-- Exercise 1:
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL lst fun) = GL (lst : emp) (fun + empFun emp)
--glCons emp lst = (GL [emp] $ empFun emp) `mappend` lst

instance Monoid GuestList where
        mempty = GL [] 0
        mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun  = max


-- Exercise 2:
treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f ini (Node val lst) = f val $ map (treeFold f ini) lst


-- Exercise 3:
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss lst = (withBoss, noBoss)
        where noBoss   = mconcat . map (uncurry moreFun) $ lst
              withBoss = (glCons boss) . mconcat . map snd $ lst
              -- moncat = fold mappend mempty
              -- (uncurry f) a b = f (a, b)


-- Exercise 4:
maxFun :: Tree Employee -> GuestList
maxFun = (uncurry moreFun) . treeFold nextLevel (mempty, mempty)


-- Exercise 5:
format :: GuestList -> [String]
format (GL lst fun) = header : body
        where header = "Total fun: " ++ show fun
              body   = sort . map empName $ lst

main :: IO ()
main = (readFile "company.txt") >>= ((mapM_ putStrLn) . format . maxFun . read)
