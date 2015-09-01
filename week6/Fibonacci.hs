{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

-- CIS 194: Homework 6
--
-- Exercise 1
fib :: Integer -> Integer
fib n | n < 2 = n
      | otherwise = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
type Current  = Integer
type Next     = Integer

fibFast :: (Current, Next) -> Integer -> Integer
fibFast (x,y) n | n == 0 = x
                | otherwise = fibFast (y,x+y) (n-1)

fibs2 :: [Integer]
fibs2 = map (fibFast (0,1)) [0..]

-- Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

instance Show a => Show (Stream a) where
    show s = show (take 20 (streamToList s))

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) $ streamMap f s

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f $ f x

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x s1) (Cons y s2) = Cons x $ Cons y $ interleaveStreams s1 s2

bCount :: Integer -> Integer
bCount = (+1) . toInteger . length . takeWhile (==0) . toBinary
    where toBinary n | n > 0 = n `mod` 2 : (toBinary $ n `div` 2)
                     | otherwise = []

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) $ streamMap bCount $ streamFromSeed (+1) 1

-- Exercise 6
x' :: Stream Integer
x' = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
    fromInteger n = Cons n $ streamRepeat 0
    negate = streamMap (* (-1))
    (+) (Cons a0 a') (Cons b0 b') = Cons (a0+b0) (a'+b')
    (*) (Cons a0 a') b@(Cons b0 b') = Cons (a0*b0) $ (streamMap (a0*) b') + a'*b

instance Fractional (Stream Integer) where
    (/) (Cons a0 a') (Cons b0 b') = q
        where q = Cons (a0 `div` b0) $ streamMap (`div` b0) (a' - q*b')

fibs3 :: Stream Integer
fibs3 = x' / (1 - x' - (x'*x'))

-- Exercise 7
data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
    (*) (Matrix a b c d) (Matrix e f g h) = Matrix (a*e+b*g) (a*f+b*h) (c*e+d*g) (c*f+d*h)

fib4 :: Integer -> Integer
fib4 n | n < 2 = n
       | otherwise = getFib $ (Matrix 1 1 1 0)^n
            where getFib (Matrix _ x _ _) = x
