{-- CIS 194: Homework6 --}
{-- https://www.seas.upenn.edu/~cis194/spring13/hw/06-laziness.pdf --}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Fibonacci where

-- Exercise 1:
fib :: Integer -> Integer
fib n
    | n < 0     = -fib(-n)
    | n <= 1    = n
    | otherwise = (fib (n-1)) + (fib (n-2))

fibs1 :: [Integer]
fibs1 = map fib [0..]


-- Exercise 2:
fibs2 :: [Integer]
fibs2 = (map fst . iterate f) (0,1)
    where f (a,b) = (b, a + b)


-- Exercise 3:
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons y ys) = [y] ++ streamToList ys

instance Show a => Show (Stream a) where
    show = show . (take 20) . streamToList

{--
instance Eq a => Eq (Stream a) where
    (==) x y = xs == ys
        where xs = take 100 $ streamToList x
              ys = take 100 $ streamToList y
--}


-- Exercise 4:
streamRepeat :: a -> Stream a
streamRepeat y = Cons y (streamRepeat y)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons y ys) = Cons (f y) (streamMap f ys)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f y = Cons y $ streamFromSeed f (f y)

{-- Convert the stream into standard list then reuse functions in Prelude
listToStream :: [a] -> Stream a
listToStream [] = error "Stream must be infinite"
listToStream (x:xs) = Cons x (listToStream xs)

streamRepeat' :: a -> Stream a
streamRepeat' = listToStream . repeat

streamMap' :: (a -> b) -> Stream a -> Stream b
streamMap' f = listToStream . (map f) . streamToList

streamFromSeed' :: (a -> a) -> a -> Stream a
streamFromSeed' f = listToStream . (iterate f)
--}


-- Exercise 5:
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = step 0
    where step i = interleaveStreams (streamRepeat i) (step (i+1))

--ruler' :: Stream Integer
--ruler' = streamMap (check . (+1)) nats
    --where check x = last (filter (\y -> x `mod` (2^y) == 0)  [0..x])

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons y ys) z = Cons y $ interleaveStreams z ys

-- wrong:
-- interleaveStreams (Cons x xs) (Cons y ys) = Cons x (Cons y (interleaveStreams xs ys))
--
-- step 0
-- {Definition of step}
-- ils (sr 0)
--     (step 1)
-- {Definition of sr}
-- ils (c 0 (sr 0))         x = 0, xs = (sr 0), (Cons y ys) = (ils ...) not resolved
--     (ils (sr 1)          (Cons x xs) = (sr 1) not resolved, (Cons y ys) = (step 2) not resolved
--          (step 2))
-- {Definition of sr}
-- ils (c 0 (sr 0))         x = 0, xs = (sr 0), (Cons y ys) = (ils ...) not resolved
--     (ils (c 1 (sr 1))    x = 1, xs = (sr 1), (cons y ys) = (step 2) not resolved
--          (step 2))
-- {Definition of step}
-- ils (c 0 (sr 0))         x = 0, xs = (sr 0), (Cons y ys) = (ils ...) not resolved
--     (ils (c 1 (sr 1))    x = 1, xs = (sr 1), (Cons y ys) = (ils ...) not resolved
--          (ils (sr 2)     (Cons x xs) = (sr 2) not resolved, (Cons y ys) = (step 3) not resolved
--               (step 3)))
-- ..... the first level can't be resolved until the (Cons y ys) in the last level is resolved

-- correct version:
-- step 0
-- {Definition of step}
-- ils (sr 0)
--     (step 1)
-- {Definition of sr}
-- ils (c 0 (sr 0))                         x = 0, xs = (sr 0), y = (step 1)
--     (step 1)
-- {Definition of ils}
-- cons 0 $ ils (step 1)
--              (sr 0)
-- {Definition of step}
-- cons 0 $ ils (ils (sr 1)
--                   (step 2))
--              (sr 0)
-- {Definition of sr}
-- cons 0 $ ils (ils (c 1 (sr 1))           x = 1, xs = (sr 1), y = (step 2)
--                   (step 2))
--              (sr 0)
-- {Definition of ils}
-- cons 0 $ ils (cons 1 $ (ils (step 2)     x = 1, xs = (ils (step 2) (sr 1)), y = (sr 0)
--                             (sr 1)))
--                        (sr 0)
-- {Definition of ils}
-- cons 0 $ cons 1 $ ils (sr 0)
--                       (ils (step 2)
--                            (sr 1))


-- Exercise 6:
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger y = Cons y (streamRepeat 0)

    negate s = streamMap negate s

    (+) (Cons y ys) (Cons z zs) = Cons (y + z) (ys + zs)

    (*) (Cons y ys) zz@(Cons z zs) = Cons (y * z)
                                          ((streamMap (*y) zs) + (ys * zz))

instance Fractional (Stream Integer) where
    (/) (Cons y ys) (Cons z zs) = q
        where q = Cons (y `div` z) (streamMap ( `div` z) (ys - q * zs))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ (2 :: Integer))


-- Exercise 7:
data Matrix = Matrix Integer Integer Integer Integer
    deriving (Show)

instance Num Matrix where
    (*) (Matrix x0 x1 x2 x3) (Matrix y0 y1 y2 y3)
        = Matrix (x0 * y0 + x1 * y2)
                 (x0 * y1 + x1 * y3)
                 (x2 * y0 + x3 * y2)
                 (x2 * y1 + x3 * y3)

    (+) (Matrix x0 x1 x2 x3) (Matrix y0 y1 y2 y3)
        = Matrix (x0+y0) (x1+y1) (x2+y2) (x3+y3)

-- Use one extra step to avoid dealing with special cases
fib4 :: Integer -> Integer
fib4 n = (signum n) * takeLast (f0 ^ (abs n + 1))
    where takeLast (Matrix _ _ _ i) = i
          f0 = Matrix 1 1 1 0
