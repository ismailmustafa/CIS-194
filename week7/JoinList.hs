{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
module JoinList where

import Data.Monoid
import Scrabble
import Sized
import Buffer
import Editor

-- CIS 194: Homework 7

data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

-- Combines two join lists
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

-- determines the size of a given join list
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- Gets value at given index of join list
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                = Nothing
indexJ _ (Single _ a)         = Just a
indexJ n jl@(Append _ l r)
    | n < 0 || n >= sizeOf jl = Nothing
    | n < (sizeOf l)     = indexJ n l
    | otherwise          = let newN = n - (sizeOf jl - sizeOf r)
                                in indexJ newN r
-- Gets size of join list
sizeOf :: (Sized b, Monoid b) => JoinList b a -> Int
sizeOf Empty = getSize mempty
sizeOf (Single m _)   = getSize $ size m
sizeOf (Append m _ _) = getSize $ size m

-- like @take@ but for join lists
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ _ jl@(Single _ _) = jl
takeJ n jl@(Append m l r)
    | n <= 0 = Empty
    | n >= (getSize $ size m) = jl
    | n <= sizeOf l = takeJ n l
    | otherwise = l +++ takeJ (n - sizeOf l) r

-- like @drop@ but for join lists
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ _ jl@(Single _ _) = jl
dropJ n jl@(Append m l r)
    | n <= 0 = jl
    | n >= (getSize $ size m) = Empty
    | n >= sizeOf l = dropJ (n - sizeOf l) r
    | otherwise = dropJ n l +++ r

-- Scrabble score for entire line
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList (Score, Size) String) where
    toString Empty          = ""
    toString (Single _ a)   = a
    toString (Append _ l r) = toString l ++ toString r
    
    fromString xs = foldr1 (+++) (map single $ lines xs)
        where single s = Single (scoreString s, Size 1) s

    line = indexJ

    replaceLine n s buf = takeJ n buf +++ fromString s +++ dropJ (n+1) buf

    numLines = sizeOf

    value = getScore . fst . tag

main :: IO ()
main = runEditor editor (fromString "testing" :: (JoinList (Score, Size) String))
