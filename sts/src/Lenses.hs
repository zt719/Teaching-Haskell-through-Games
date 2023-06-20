{-# LANGUAGE RankNTypes #-}

module Lenses where

import Data
import MTL

import Data.Functor.Identity
import Data.Functor.Const

-- Simple Definition of Lens --
type Lens b a = forall f. Functor f => (a -> f a) -> b -> f b

-------------------------------------------------------------------------------

-- Getter --
view :: Lens b a -> b -> a
view lens b = getConst ((lens Const) b)

(^.) :: b -> Lens b a -> a
b ^. lens = view lens b
infixl 8 ^.

-- Setter --
set :: Lens b a -> a -> b -> b
set lens a b = over lens (const a) b

(.~) = set
infixr 4 .~

-- Over --
over :: Lens b a -> (a -> a) -> b -> b
over lens f = runIdentity . lens (Identity . f)

(%~) = over
infixr 4 %~

-- Helper function for better syntax
(&) :: a -> (a -> b) -> b
x & f = f x
infixl 1 &

-------------------------------------------------------------------------------

-- Lenses for 'Battle' --
player :: Lens Battle Unit
player f b = fmap (\a -> b { _player = a }) $ f (_player b)

enemies :: Lens Battle [Unit]
enemies f b = fmap (\a -> b { _enemies = a }) $ f (_enemies b)

mana :: Lens Battle Int
mana f b = fmap (\a -> b { _mana = a }) $ f (_mana b)

turn :: Lens Battle Int
turn f b = fmap (\a -> b { _turn = a }) $ f (_turn b)

cardPool :: Lens Battle CardPool
cardPool f b = fmap (\a -> b { _cardPool = a }) $ f (_cardPool b)

-- Lenses for 'Card' --
cardName :: Lens Card CardName
cardName f b = fmap (\a -> b { _cardName = a }) $ f (_cardName b)

cardType :: Lens Card CardType
cardType f b = fmap (\a -> b { _cardType = a }) $ f (_cardType b)

cardCost :: Lens Card Int
cardCost f b = fmap (\a -> b { _cardCost = a }) $ f (_cardCost b)

cardInfo :: Lens Card String
cardInfo f b = fmap (\a -> b { _cardInfo = a }) $ f (_cardInfo b)

-- Lenses for 'Unit' --
name :: Lens Unit Name
name f b = fmap (\a -> b { _name = a }) $ f (_name b)

maxHealth :: Lens Unit Int
maxHealth f b = fmap (\a -> b { _maxHealth = a }) $ f (_maxHealth b)

health :: Lens Unit Int
health f b = fmap (\a -> b { _health = a }) $ f (_health b)

block :: Lens Unit Int
block f b = fmap (\a -> b { _block = a }) $ f (_block b)

strength :: Lens Unit Int
strength f b = fmap (\a -> b { _strength = a }) $ f (_strength b)

dexterity :: Lens Unit Int
dexterity f b = fmap (\a -> b { _dexterity = a }) $ f (_dexterity b)

weak :: Lens Unit Int
weak f b = fmap (\a -> b { _weak = a }) $ f (_weak b)

vulnerable :: Lens Unit Int
vulnerable f b = fmap (\a -> b { _vulnerable = a }) $ f (_vulnerable b)

frail :: Lens Unit Int
frail f b = fmap (\a -> b { _frail = a }) $ f (_frail b)

intends :: Lens Unit (Int -> [Intend])
intends f b = fmap (\a -> b { _intends = a }) $ f (_intends b)

-- Lenses for 'CardPool' --
drawPile :: Lens CardPool [Card]
drawPile f b = fmap (\a -> b { _drawPile = a }) $ f (_drawPile b)

hand :: Lens CardPool [Card]
hand f b = fmap (\a -> b { _hand = a }) $ f (_hand b)

discardPile :: Lens CardPool [Card]
discardPile f b = fmap (\a -> b { _discardPile = a }) $ f (_discardPile b)


-- An unsafe version of list lens --
at :: Int -> Lens [a] a
at n f as = fmap (\x -> aux n x as) $ f (as !! n)
  where
    aux :: Int -> a -> [a] -> [a]
    aux 0 x (_:as) = x : as
    aux n x (a:as) = a : (aux (n-1) x as)

-------------------------------------------------------------------------------

-- Lens State Combinators: --
-- Setter for State --
assign :: MonadState s m => Lens s a -> a -> m ()
assign lens a = modify (set lens a)

(.=) :: MonadState s m => Lens s a -> a -> m ()
(.=) = assign
infix 4 .=

-- Over for State --
modifying :: MonadState s m => Lens s a -> (a -> a) -> m ()
modifying l f = modify (over l f)

(%=) :: MonadState s m => Lens s a -> (a -> a) -> m ()
(%=) = modifying
infix 4 %=

-- Getter for State --
use :: MonadState s m => Lens s a -> m a
use lens = get >>= \s -> return (view lens s) 

-- Incrementer for Numerable State
(+=) :: (MonadState s m, Num a) => Lens s a -> a -> m ()
l += a = modifying l (+a)
infix 4 +=
  
-- Lens Writer Combinators: -- 
scribe :: (MonadWriter w m) => Lens w a -> a -> m ()
scribe lens b = tell (set lens b mempty)

-------------------------------------------------------------------------------

-- Lenses for 'Relics' --

suriken :: Lens Relics Int
suriken f b = fmap (\a -> b { _suriken = a }) $ f (_suriken b)

kunai :: Lens Relics Int
kunai f b = fmap (\a -> b { _kunai = a }) $ f (_kunai b)

-- Lenses for 'Logger' --

history :: Lens Logger String
history f b = fmap (\a -> b { _history = a }) $ f (_history b)

cardsPlayed :: Lens Logger (Sum Int)
cardsPlayed f b = fmap (\a -> b { _cardsPlayed = a }) $ f (_cardsPlayed b)

