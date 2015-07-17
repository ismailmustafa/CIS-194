{-# OPTIONS_GHC -Wall #-}
module Golf where

import qualified Data.List as L
import qualified Data.Ord as O
import qualified Data.Char as C

-- CIS 194: Homework 3

-- Exercise 1
--
-- Hopscotch
skips :: [a] -> [[a]]
skips [] = []
skips l = [l] ++ map (\x -> sk $ zip3 l [1..n] $ replicate n x) [2..n]
    where
        sk lt = [f | (f,s,t) <- lt, mod s t == 0]
        n = length l

-- Exercise 2
--
-- Local Maxima
localMaxima :: [Integer] -> [Integer]
localMaxima xs = [s | (f,s,t) <- threes xs, s>t && s>f]

threes :: [Integer] -> [(Integer,Integer,Integer)]
threes (x:y:z:zs) = (x,y,z) : threes (y:z:zs)
threes _          = []

-- Exercise 3
--
-- Histogram
histogram :: [Integer] -> String
histogram xs = unlines $ (map hLToS $ hData xs)++[replicate 10 '=']++[['0'..'9']]

hCount :: [Integer] -> [(Integer,Integer)]
hCount xs = [(x,L.genericLength l) | l@(x:_) <- L.group $ L.sort xs]

hMax :: [(Integer,Integer)] -> (Integer,Integer)
hMax = L.maximumBy (O.comparing snd)

hLine :: [(Integer,Integer)] -> Integer -> [Integer]
hLine tl n = [f | (f,s) <- tl, s > n]

hData :: [Integer] -> [[Integer]]
hData xs = reverse $ map (hLine $ hCount xs) [0..(snd $ hMax $ hCount xs)-1]

hLToS :: [Integer] -> String
hLToS xs = [if elem (toInteger $ C.digitToInt x) xs then '*' else ' ' | x <- ['0'..'9']]
