module Game where

import System.Random (StdGen, getStdGen, Random(randomR), random, randoms)

data Ball = R | B | G | Y
  deriving (Eq, Enum, Show)

instance Random Ball where
  random g = case randomR (0, 3) g of (r, g') -> (toEnum r, g')
  randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of (r, g') -> (toEnum r, g')

trackLen :: Int
trackLen = 30

class Game a where
  getList    :: a -> [Ball]
  getPtr     :: a -> Int
  moveLeft   :: a -> a
  moveRight  :: a -> a
  shootBall  :: Ball -> a -> a
  appendBall :: Ball -> a -> (a, Bool)
