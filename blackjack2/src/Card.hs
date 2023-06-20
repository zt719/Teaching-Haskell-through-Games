module Card where

import System.Random
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.List (nub, sort)


data Suit = Spade | Heart | Club | Diamond
  deriving (Eq, Enum)

instance Show Suit where
  show Spade = "♠"
  show Heart = "♥"
  show Club  =  "♣"
  show Diamond = "♦"


data Point = Ace | Two | Three | Four | Five
  | Six | Seven | Eight | Nine | Ten 
  | Jack | Queen | King
  deriving (Eq, Enum)

instance Show Point where
  show Ace = "A"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show point = show (fromEnum point + 1)

data Card = Card
  { _suit  :: Suit
  , _point :: Point
  } deriving (Eq)

instance Show Card where
  show (Card s p) = "[" ++ show s ++ show p ++ "]"


evalHand :: [Card] -> Int
evalHand = bestMatch . evalHand' . map _point 

evalHand' :: [Point] -> [Int]
evalHand' [] = [0]
evalHand' (Ace : cs) = 
  let evalRest = evalHand' cs in
    nub $ map (+1) evalRest ++ map (+11) evalRest
evalHand' (Jack : cs)  = map (+10) (evalHand' cs)
evalHand' (Queen : cs) = map (+10) (evalHand' cs)
evalHand' (King : cs)  = map (+10) (evalHand' cs)
evalHand' (c : cs)     = map (+(fromEnum c + 1)) (evalHand' cs)

splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy f [] = ([], [])
splitBy f (a:as)
  | f a    = (a : hs, ts) 
  | otherwise = (hs, a : ts)
  where
    (hs, ts) = splitBy f as

bestMatch :: [Int] -> Int
bestMatch as = 
  let (hs, ts) = splitBy (<=21) as in
    case hs of
      [] -> case ts of
              [] -> 0
              _  -> head (sort ts)
      _  -> head (reverse (sort (hs)))

fullDeck :: [Card]
fullDeck = [Card suit point | suit <- [Spade ..], point <- [Ace ..]]

shuffle :: [a] -> StdGen -> ([a],StdGen)
shuffle xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs =  newListArray (1,n) xs

