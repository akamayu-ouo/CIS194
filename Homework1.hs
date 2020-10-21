{-- CIS 194: Homework1 --}
{-- https://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf --}
-- Exercise 1:

{- Use library function
import Data.Char
toDigits :: Integer -> [Integer]
toDigits x
    | x > 0 = map (toInteger . digitToInt) $ show x
    | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits
-}

toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = toDigits (x `div` 10)  ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = [x `mod` 10] ++ toDigitsRev (x `div` 10)

-- Exercise 2:
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst = zipWith (*) (cycle c) lst
  where c = if (odd . length $ lst) 
            then [1,2]
            else [2,1]

-- Exercise 3:
sumDigits :: [Integer] -> Integer
sumDigits x = foldr (+) 0 $ map f x
  where f x = x `div` 10 + x `mod` 10

-- Exercise 4:
validate :: Integer -> Bool
validate x = 0 == (checksum `mod` 10)
  where checksum = sumDigits . doubleEveryOther . toDigits $ x

validate' :: Integer -> Bool
validate' x = 0 == (checksum `mod` 10)
    where checksum = sumDigits . doubleEveryOther' . toDigitsRev $ x
            where doubleEveryOther' = zipWith (*) (cycle [1,2]) 

-- Exercise 5:
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, c)] ++ (hanoi (n - 1) b a c)
