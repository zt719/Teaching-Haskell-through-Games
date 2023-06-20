{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Battle where

import Lenses
import MTL
import Data

import Data.Functor.Identity
import Data.Functor.Const

------------------------------------------------------------------------------

class Monad m => MonadBattle m where

  startBattle     :: Battle -> m Battle
  endBattle       :: Battle -> m Battle
  startTurn       :: Battle -> m Battle
  endTurn         :: Battle -> m Battle
  startETurn      :: Battle -> m Battle
  endETurn        :: Battle -> m Battle
  playCardOnEnemy :: Int  -> Int -> Battle -> m Battle
  applyCardEffect :: CardName -> Int -> Battle -> m Battle

  startBattle = startTurn

  endBattle = return
    . over player unitResetStatuses
    . over cardPool resetCardPool

  startTurn = return
    . set mana 3
    . set (player . block) 0
    . over turn succ
    . over cardPool (applyNTimes 5 drawFromPile)

  endTurn = return
    . over player unitDecStatuses
    . over cardPool discardHand

  startETurn battle_ =
    return $ foldr (.) id (reverse $ map applyIntends [0.. length (battle_ ^. enemies) - 1]) battle_

  endETurn = return . over enemies (map unitDecStatuses)

  playCardOnEnemy cIndex_ eIndex_ battle_ = do
    let card_ = (battle_ ^. cardPool . hand) !! cIndex_
    if card_ ^. cardCost <= battle_ ^. mana
      then do
        battle_ <- return $ battle_ & mana %~ (+(-card_ ^. cardCost))
        battle_ <- return $ battle_ & cardPool %~ discard cIndex_
        applyCardEffect (card_ ^. cardName) eIndex_ battle_
    else return battle_

  applyCardEffect Strike n_ battle_ = do
    let d  = unitGetDamage 6 (battle_ ^. player)
    battle_ <- return $ battle_ & enemies . at n_ %~ unitTakeDamage d
    return battle_
  applyCardEffect Defend n_ battle_ = do
    battle_ <- return $ battle_ & player %~ playerGetBlock 5
    return battle_
  applyCardEffect Bash n_ battle_ = do
    let d = unitGetDamage 8 (battle_ ^. player) 
    battle_ <- return $ battle_ & enemies . at n_ %~ unitTakeDamage d
    battle_ <- return $ battle_ & enemies . at n_ . vulnerable %~ (+2)
    return battle_

 
-------------------------------------------------------------------------------

instance MonadBattle Identity

-------------------------------------------------------------------------------

instance MonadBattle IO

-------------------------------------------------------------------------------

surikenEffect :: MonadBattle m => Battle -> StateT Relics m Battle
surikenEffect battle_ = do
  suriken_ <- use suriken
  case suriken_ of
    2  -> do
      assign suriken 0
      return $ battle_ & player . strength %~ (+1)
    _  -> do
      modifying suriken succ
      return battle_

kunaiEffect :: MonadBattle m => Battle -> StateT Relics m Battle
kunaiEffect battle_ = do
  kunai_ <- use kunai
  case kunai_ of
    2  -> do
      assign kunai 0
      return $ battle_ & player . dexterity %~ (+1)
    _  -> do
      modifying kunai succ
      return battle_

instance MonadBattle m => MonadBattle (StateT Relics m) where

  applyCardEffect cardName_ eIndex_ battle_ = do
    battle <- lift $ applyCardEffect cardName_ eIndex_ battle_
    if getTypeByName cardName_ == Attack then do
      battle <- surikenEffect battle
      battle <- kunaiEffect battle
      return battle
    else do
      return battle
      
instance MonadBattle m => MonadBattle (WriterT Logger m) where
  applyCardEffect cardName_ eIndex_ battle_ = do
    scribe history $ "You played: " ++ show cardName_ ++ "\n"
    scribe cardsPlayed $ Sum 1
    lift $ applyCardEffect cardName_ eIndex_ battle_


-------------------------------------------------------------------------------

-- Pure Helper Functions --

getTypeByName :: CardName -> CardType
getTypeByName Strike = Attack
getTypeByName Defend = Skill
getTypeByName Bash   = Attack

applyIntends :: Int -> Battle -> Battle
applyIntends eIndex_ battle_ =
  foldr (.) id (reverse $ zipWith applyIntend intends_ $ repeat eIndex_) battle_
    where
      intends_ = (battle_ ^. enemies . at eIndex_ . intends) (battle_ ^. turn)

applyIntend :: Intend -> Int -> Battle -> Battle
applyIntend (DealDamage baseDamage) eIndex_ battle_ = over player (unitTakeDamage (unitGetDamage baseDamage (battle_ ^. enemies . at eIndex_))) battle_
applyIntend (ApplyVulnerable layer) eIndex_ battle_ = over (player . vulnerable) (+layer) battle_
applyIntend (ApplyWeak       layer) eIndex_ battle_ = over (player . weak) (+layer) battle_
applyIntend (ApplyFrail      layer) eIndex_ battle_ = over (player . frail) (+layer) battle_
applyIntend (GainStrength    layer) eIndex_ battle_ = over (enemies . at eIndex_ . strength) (+layer) battle_
applyIntend (GainBlock       layer) eIndex_ battle_ = over (enemies . at eIndex_ . block) (+layer) battle_
applyIntend NoIntend                eIndex_ battle_ = battle_


unitGetDamage :: Int -> Unit -> Int
unitGetDamage baseDamage unit_
  | unit_ ^. weak > 0 = damage * 3 `div` 4
  | otherwise         = damage
  where
    damage = baseDamage + unit_ ^. strength

unitTakeDamage :: Int -> Unit -> Unit
unitTakeDamage baseDamage unit_
  | diff >= 0 = set block diff unit_
  | otherwise = set block 0 (over health (+diff) unit_)
  where
    diff   = unit_ ^. block - damage
    damage = if unit_ ^. vulnerable > 0
      then baseDamage * 3 `div` 2
      else baseDamage

playerGetBlock :: Int -> Unit -> Unit
playerGetBlock baseBlock unit_
  | unit_ ^. frail > 0 = over block (+(block_ * 3 `div` 4)) unit_
  | otherwise          = over block (+block_) unit_
  where
    block_ = baseBlock + unit_ ^. dexterity

unitResetStatuses :: Unit -> Unit
unitResetStatuses =
  set weak 0 . set vulnerable 0 . set frail 0

unitDecStatuses :: Unit -> Unit
unitDecStatuses =
  over weak dec . over vulnerable dec . over frail dec
    where
      dec 0 = 0
      dec n = n - 1

drawFromPile :: CardPool -> CardPool
drawFromPile cp@(CardPool a b c)
  | length b == 10 = cp
  | null a         = if null c then cp else drawFromPile (shuffle cp)
  | otherwise      = let card:a' = a in CardPool a' (b ++ [card]) c
  where
    shuffle (CardPool a b c) = CardPool (a ++ reverse c) b []

discardHand :: CardPool -> CardPool
discardHand (CardPool a b c) = CardPool a [] (c ++ b)

discard :: Int -> CardPool -> CardPool
discard n (CardPool a b c) =
  let (card, b') = aux n b in CardPool a b' (c ++ [card])
    where
      aux :: Int -> [Card] -> (Card, [Card])
      aux 0 (c:cs) = (c, cs)
      aux n (c:cs) = let (c', cs') = aux (n-1) cs in (c', c:cs')

resetCardPool :: CardPool -> CardPool
resetCardPool (CardPool a b c) = CardPool (a ++ b ++ c) [] []

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 _ a = a
applyNTimes n f a = f (applyNTimes (n-1) f a)

-------------------------------------------------------------------------------




