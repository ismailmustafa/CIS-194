{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Scrabble where

import Data.Char

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
    mempty = Score 0
    mappend = (+)

score :: Char -> Score
score c
        | lower `elem` "aeilnorstu" = Score 1
        | lower `elem` "dg" = Score 2
        | lower `elem` "bcmp" = Score 3
        | lower `elem` "fhvwy" = Score 4
        | lower `elem` "k" = Score 5
        | lower `elem` "jx" = Score 8
        | lower `elem` "qz" = Score 10
        | otherwise = Score 0
            where lower = toLower c


scoreString :: String -> Score
scoreString = foldr (mappend . score) $ Score 0
