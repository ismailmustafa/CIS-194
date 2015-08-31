{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Calc06 where

import qualified Data.Map as M

-- Exercise 6
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
              | Var String
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
    deriving (Show, Eq)

instance Expr VarExprT where
    lit = Lit
    add = Add
    mul = Mul

instance HasVars VarExprT where
    var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit = \x _ -> Just x
    add = \e1 e2 env -> case (e1 env) of
                            Just x -> case (e2 env) of
                                        Just y -> Just $ x+y
                                        Nothing -> Nothing
                            Nothing -> Nothing
    mul = \e1 e2 env -> case (e1 env) of
                            Just x -> case (e2 env) of
                                        Just y -> Just $ x*y
                                        Nothing -> Nothing
                            Nothing -> Nothing

withVars :: [(String, Integer)] 
         -> (M.Map String Integer -> Maybe Integer) 
         -> Maybe Integer
withVars vs e = e $ M.fromList vs
