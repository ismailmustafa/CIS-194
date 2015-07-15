{-# OPTIONS_GHC -Wall #-}

-- CIS 194: Homework 1

-- Exercise 1
--
-- Convert integer to list of digits
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = (toDigits . floor $ (convertedValue) / 10.0)++[n `mod` 10 ]
        where
            convertedValue = fromIntegral n :: Double

-- Same as toDigits but reversed
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse . toDigits $ n

-- Exercise 2
--
-- Double every other value starting with penultimate value
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs
    | len == 0          = []
    | len `mod` 2 == 0  = (head xs * 2):(doubleEveryOther . tail $ xs)
    | otherwise         = (head xs):(doubleEveryOther . tail $ xs)
        where 
            len = length xs

-- Exercise 3
--
-- Break multiple digit numbers into list and sum
sumMultiDigit :: Integer -> Integer
sumMultiDigit n = foldl (+) 0 $ toDigits n

-- Sum list of numbers (breaking apart multiple digit numbers)
sumDigits :: [Integer] -> Integer
sumDigits xs = foldl (+) 0 $ map sumMultiDigit xs

-- Exercise 4
--
-- Combine all previous functions to validate credit card number
validate :: Integer -> Bool
validate n
    | checkSum `mod` 10 == 0 = True
    | otherwise              = False
        where
            checkSum = sumDigits . doubleEveryOther $ toDigits n

-- Exercise 5
--
-- Implementation of Towers of Hanoi Problem
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n > 0     = (hanoi (n-1) a c b)++[(a,b)]++(hanoi (n-1) c b a)
    | otherwise = []

main :: IO ()
main = do
    putStrLn . show $ validate 4012888888881881
    putStrLn . show $ validate 4012888888881882

