{-# LANGUAGE FlexibleInstances #-}

module List where

import Game

append :: a -> [a] -> [a]
append a as = a : as

insert :: Int -> a -> [a] -> [a]
insert 0 x as       = x : as
insert n x (a : as) = a : insert (n - 1) x as

reduce :: Eq a => Int -> [a] -> [a]
reduce n as = ungroup (reduce' n (group as))

reduce' :: Eq a => Int -> [[a]] -> [[a]]
reduce' _ [] = []
reduce' n (as : ass)
  | n < asLen = if asLen >= 3 then ass else as : ass
  | otherwise = if theTail == as : ass then theTail else reduce' asLen theTail
    where
      asLen   = length as
      theTail = merge as (reduce' (n - asLen) ass)

merge :: Eq a => [a] -> [[a]] -> [[a]]
merge as' [] = [as']
merge as' (as : ass)
  | head as' == head as = (as' ++ as) : ass
  | otherwise           = as' : as : ass

group :: Eq a => [a] -> [[a]]
group [] = []
group [a] = [[a]]
group (a : as)
  | elem a a' = (a : a') : as'
  | otherwise = [a] : a' : as'
    where
      (a' : as') = group as

ungroup :: [[a]] -> [a]
ungroup [] = []
ungroup (a : as) = a ++ ungroup as

data ListPtr a = ListPtr [a] Int

instance Game (ListPtr Ball) where

  getList (ListPtr chain _) = chain

  getPtr (ListPtr _ ptr) = ptr

  moveLeft (ListPtr chain ptr)
    | ptr == 0  = ListPtr chain ptr
    | otherwise = ListPtr chain (ptr - 1)

  moveRight (ListPtr chain ptr)
    | ptr == trackLen = ListPtr chain ptr
    | otherwise       = ListPtr chain (ptr + 1)

  shootBall ball (ListPtr chain ptr)
    | ptr <= length chain = ListPtr (reduce ptr (insert ptr ball chain)) ptr
    | otherwise           = ListPtr chain ptr
  
  appendBall ball (ListPtr chain ptr)
    | length chain > trackLen = (ListPtr chain ptr, False)
    | otherwise               = (ListPtr (ball : chain) ptr, True)
