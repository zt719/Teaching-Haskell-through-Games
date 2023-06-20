{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import AlgEff
import Card
import System.Random
import Control.Monad.State.Class
import Control.Monad.Fail

main :: IO ()
main = do
  gen <- getStdGen
  let deck = fst (shuffle fullDeck gen)
  putStrLn "Player's turn:"
  playerResult <- runBlackjack (loopPlayer deck)
  case playerResult of
    Left message -> putStrLn message
    Right (playerHand, deck) -> do
      putStrLn "Your hand:"
      putStrLn (show playerHand)
      putStrLn "Dealer's turn:"
      dealerResult <- runBlackjack (loopDealer deck)
      case dealerResult of
        Left message -> putStrLn message
        Right (dealerHand, deck) -> do
          putStrLn "Dealer's hand:"
          putStrLn (show dealerHand)
          if evalHand playerHand > evalHand dealerHand then
            putStrLn "Player won"
          else
            putStrLn "Dealer won"

instance MonadState [Card] (Free (StateSig [Card] :+: IOSig)) where
  get = left get
  put = left . put

instance MonadIO (Free (StateSig [Card] :+: IOSig)) where
  getL = right getL
  putL = right . putL

loopPlayer :: [Card] -> Free (StateSig [Card] :+: IOSig) ([Card], [Card])
loopPlayer (c:cs) = do
  putL "Continue?(y/n)"
  str <- getL
  if str == "y" then do
    putL (show c)
    dealCard c
    loopPlayer cs
  else if str == "n" then do
    hand <- get
    return (hand, (c:cs))
  else
    loopPlayer (c:cs)

loopDealer :: [Card] -> Free (StateSig [Card] :+: IOSig) ([Card], [Card])
loopDealer (c:cs) = do
  hand <- get
  if evalHand hand >= 17 then do
    hand <- get
    return (hand, (c:cs))
  else do
    putL (show c)
    dealCard c
    loopDealer cs


dealCard :: Functor sig => Card -> Free (StateSig [Card] :+: sig) ()
dealCard c = left get >>= left . put . (c:) 

algBustedHand :: Functor sig =>
  StateSig [Card] (Free (FailSig :+: sig) ([Card] -> Free (FailSig :+: sig) a)) ->
  Free (FailSig :+: sig) ([Card] -> Free (FailSig :+: sig) a)
algBustedHand (Get k) = return (\s -> k s >>= \f -> f s)
algBustedHand (Put s k)
  | evalHand s > 21 = Op (L (FailSig ("Busted! " ++ show (evalHand s) ++ " points")))
  | otherwise       = return (\_ -> k () >>= \f -> f s)

handleBlackjack :: Functor sig =>
  Free (StateSig [Card] :+: sig) a ->
  Free (FailSig :+: sig) ([Card] -> Free (FailSig :+: sig) a)
handleBlackjack = fold genMS (algBustedHand \/ fwdR)

runBlackjack :: Free (StateSig [Card] :+: IOSig) a -> IO (Either String a)
runBlackjack = foldIO . foldMF' . finallyMS [] . handleBlackjack

runBlackjack' :: Free (StateSig [Card] :+: VoidSig) a -> Maybe a
runBlackjack' = foldV . foldMF . finallyMS [] . handleBlackjack

program :: [Card] -> Free (StateSig [Card] :+: VoidSig) a
program (c:cs) = dealCard c >> program cs
