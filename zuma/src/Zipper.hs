{-# LANGUAGE FlexibleInstances #-}

module Zipper where

import Game

data Zipper a = Zipper [a] [a]
  deriving (Show)

fromList :: [a] -> Zipper a
fromList as = Zipper [] as

toList :: Zipper a -> [a]
toList (Zipper ls rs) = reverse ls ++ rs

append :: a -> Zipper a -> Zipper a
append a (Zipper ls rs) = Zipper (ls ++ [a]) rs

left :: Zipper a -> Zipper a
left (Zipper (l:ls) rs) = Zipper ls (l:rs)
left (Zipper [] rs) = Zipper [] rs

right :: Zipper a -> Zipper a
right (Zipper ls (r:rs)) = Zipper (r:ls) rs
right (Zipper ls []) = Zipper ls []

insert :: a -> Zipper a -> Zipper a
insert a (Zipper ls rs) = Zipper (a:ls) rs

reduce :: Eq a => Zipper a -> Zipper a
reduce (Zipper ls rs)
  | length sequence >= 3 &&
    all (== (head sequence)) sequence = reduce (Zipper tls trs)
  | length hls >= 3 = reduce (Zipper tls trs)
  | otherwise = Zipper ls rs
  where
    sequence = hls ++ hrs
    (hls, tls) = groupHead [] ls
    (hrs, trs) = groupHead [] rs

groupHead :: Eq a => [a] -> [a] -> ([a], [a])
groupHead [] [] = ([], [])
groupHead [] (t:ts) = groupHead [t] ts
groupHead hs [] = (hs, [])
groupHead hs (t:ts)
  | elem t hs = groupHead (t:hs) ts
  | otherwise = (hs, (t:ts))
 
instance Game (Zipper Ball) where

  getList = toList
  
  getPtr (Zipper ls rs) = length ls - 1
  
  moveLeft = left
  
  moveRight = right
  
  shootBall ball = reduce . insert ball
  
  appendBall ball zipper@(Zipper ls rs)
    | length ls + length rs > 20 = (zipper, False)
    | otherwise                        = (append ball zipper, True)
