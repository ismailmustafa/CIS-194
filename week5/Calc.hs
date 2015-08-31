{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Calc where

import ExprT
import Parser
import qualified StackVM as S

-- CIS 194: Homework 5

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit x)   = x
eval (Mul x y) = eval x * eval y
eval (Add x y) = eval x + eval y

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr x = case maybeExp x of 
                Just e -> Just $ eval e
                _      -> Nothing
    where
        maybeExp = parseExp Lit Add Mul

-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

-- Exercise 4
instance Expr Integer where
    lit = id
    add x y = x + y
    mul x y = x * y

instance Expr Bool where
    lit x 
        | x <= 0    = False
        | otherwise = True
    add x y = x || y
    mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = lit $ max x y
    mul (MinMax x) (MinMax y) = lit $ min x y

instance Expr Mod7 where
    lit x   = Mod7 $ mod x 7
    add (Mod7 x) (Mod7 y) = lit (x + y)
    mul (Mod7 x) (Mod7 y) = lit (x * y)

-- Exercise 5
instance Expr S.Program where
    lit x   = [S.PushI x]
    add x y = x ++ y ++ [S.Add]
    mul x y = x ++ y ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul 

compute :: String -> Either String S.StackVal
compute x = case maybeExp of 
                Just e -> S.stackVM e
                _      -> Left "error"
                where maybeExp = compile x 
