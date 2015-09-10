{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee
import Data.Tree

-- CIS 194: Homework 8
-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp _ f) (GL xs f') = GL (emp:xs) (f+f')

instance Monoid GuestList where
    mempty  = GL [] 0
    mappend (GL xs f) (GL ys f') = GL (xs++ys) (f+f')

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node v l) = f v (map (treeFold f) l)

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss = max' . includeBoss boss
    where
        includeBoss _ []           = []
        includeBoss emp ((f,s):xs) = (glCons emp s, f) : includeBoss emp xs

max' :: [(GuestList, GuestList)] -> (GuestList, GuestList)
max' [] = (mempty, mempty)
max' xs = (maximum $ map fst xs, maximum $ map snd xs)

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- Exercise 5
main :: IO ()
main = readFile "company.txt" >>= putStrLn . processInput

processInput :: String -> String
processInput = prettyString . maxFun . read

prettyString :: GuestList -> String
prettyString (GL l f) = "Total fun: " ++ show f ++ procEmp l
    where
        procEmp [] = ""
        procEmp (x:xs) = "\n" ++ empName x ++ procEmp xs
