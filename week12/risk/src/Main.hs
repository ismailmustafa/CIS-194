module Main where

import Risk
import Control.Monad.Random
import Control.Monad
import qualified Data.List as L
import Numeric

-- Exercise 2
--
-- Define battle
battle :: Battlefield -> Rand StdGen Battlefield
battle b = do
    attRoll <- rollDie $ fst $ numDiceRolls b
    defRoll <- rollDie $ snd $ numDiceRolls b
    let sortedAttackers = sortDesc attRoll
        sortedDefenders = sortDesc defRoll
        (a,d) = unitsLost $ L.zip sortedAttackers sortedDefenders
    return $ Battlefield {attackers = attackers b - a, defenders = defenders b - d}
        where
            sortDesc = L.sortBy $ flip compare

-- Roll die n times
rollDie :: Int -> Rand StdGen [Int]
rollDie 0 = return []
rollDie n = do
    v <- die
    vs <- rollDie (n-1)
    return $ unDV v : vs

-- Takes dice rolls and return number of units
-- lost for attacker and defender
countUnits :: [(Army, Army)] -> [(Army, Army)]
countUnits [] = []
countUnits ((a,d):xs)
    | d >= a    = (1,0) : countUnits xs
    | otherwise = (0,1) : countUnits xs

unitsLost :: [(Army, Army)] -> (Army, Army)
unitsLost u = (sum $ map fst units, sum $ map snd units)
    where units = countUnits u

-- Returns the number of dice to be rolled for
-- each the attacker and defender based on the
-- number of units they have
numDiceRolls :: Battlefield -> (Int, Int)
numDiceRolls b = (numAttackers b, numDefenders b)

numAttackers :: Battlefield -> Int
numAttackers b
    | a <= 1    = 0
    | a <= 3    = a - 1
    | otherwise = 3
        where a = attackers b

numDefenders :: Battlefield -> Int
numDefenders b
    | a <= 0    = 0
    | a == 1    = a
    | otherwise = 2
        where a = defenders b

-- Exercise 3
--
-- Define invade
invade :: Battlefield -> Rand StdGen Battlefield
invade b
    | a >= 2 && d /= 0 = do 
        bt  <- battle b
        inv <- invade bt
        return $ inv
    | otherwise = do return b
        where a = attackers b
              d = defenders b

-- Exercise 4
--
-- Calculate Attack probability
successProb :: Battlefield -> Rand StdGen Double
successProb b = successProbHelper1 1000 b
    
successProbHelper1 :: Int -> Battlefield -> Rand StdGen Double
successProbHelper1 n b = successProbHelper2 n (fromIntegral n :: Double) b

successProbHelper2 :: Int -> Double -> Battlefield -> Rand StdGen Double
successProbHelper2 0 m b = do return 0.0
successProbHelper2 n m b = do
    bat <- invade b
    let a = attackers bat
        d = defenders bat
    currentTot <- successProbHelper2 (n-1) m b
    if d == 0 then (return (currentTot + (1.0/m))) else (return currentTot)

-- Set number of significant figures
formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

main :: IO ()
main = do
  let b = Battlefield {attackers=20, defenders=20}
  r <- evalRandIO $ successProb b
  let prob = formatFloatN r 3
  print prob
