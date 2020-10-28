{-- CIS 194: Homework3 --}
{-- https://www.seas.upenn.edu/~cis194/spring13/hw/03-rec-poly.pdf --}

module Golf where
import Data.List 

-- Exercise 1: Hopscotch ---------------------------------------

-- only get the n*k th elements in the list, k in [1..]
skipn :: [a] -> Int -> [a]
skipn lst n = map snd $ filter (check . fst) (zip [1..] lst)
  where check x = x `mod` n == 0

skips :: [a] -> [[a]]
skips lst = map (skipn lst) [1..(length lst)]



-- Exercise 2: Local maxima ------------------------------------

-- check if i th element is smaller than i+1 th element
localIncrease :: [Integer] -> [Bool]
localIncrease lst = [False] ++ zipWith (<) lst (tail lst)

localMaxima :: [Integer] -> [Integer]
localMaxima lst = [x | (l, x, r) <- zip3 lhs lst rhs, l && r]
  where lhs = localIncrease lst
        rhs = localIncrease $ reverse lst



-- Exercise 3: Histogram ---------------------------------------

-- count [0..9] occur how many times in the list
count :: [Integer] -> [Int]
count lst = map countNum [0..9]
  where countNum n = length $ filter (== n) lst


-- Rowwise 
visualize :: [Int] -> [String]
visualize lst = reverse $ map makeRow [1 .. h]
  where h = maximum lst
        makeRow n = map (\x -> if x >= n then '*' else ' ') lst

histogram :: [Integer] -> String
histogram = unlines . post . visualize . count
  where post lst = lst ++ ["=========="] ++ ["0123456789"]


-- Columnwise 
visualize' :: [Int] -> [String]
visualize' lst = map makeColumn $ zip [0..] lst
  where len = maximum lst
        makeColumn (n, cnt) = (replicate (len - cnt) ' ') ++ (replicate cnt '*') ++ "=" ++ (show n)

histogram' :: [Integer] -> String
histogram' = unlines . transpose . visualize' . count
