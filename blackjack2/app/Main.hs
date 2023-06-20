module Main where

import Control.Effect.State
import Control.Effect.Fail
import Control.Carrier.State.Strict
import Control.Carrier.Fail.Either
import Control.Effect.Error
import Control.Carrier.Error.Either

import Control.Monad.IO.Class

import Card
import System.Random (getStdGen)

main :: IO ()
main = do
  gen <- getStdGen
  let deck = fst (shuffle fullDeck gen)
  putStrLn "Player's turn:"
  playerResult <- (runError . runState [] $ loopPlayer deck :: IO (Either [Card] ([Card], [Card])))
  case playerResult of
    Left bustedHand          -> do
      putStrLn (show bustedHand)
    Right (playerHand, deck) -> do
      putStrLn "Your hand:"
      putStrLn (show playerHand)
      putStrLn "Dealer' turn:"
      dealerResult <- (runError . runState[] $ loopDealer deck ::IO (Either [Card] ([Card], [Card])))
      case dealerResult of
        Left bustedHand -> do
          putStrLn (show bustedHand)
        Right (dealerHand, _) -> do
          putStrLn "Dealer's hand:"
          putStrLn (show dealerHand)
          if evalHand playerHand > evalHand dealerHand then
            putStrLn "Player won"
          else
            putStrLn "Dealer won"

------------------

dealCard :: (Has (State [Card]) sig m, Has (Error [Card]) sig m, MonadIO m) =>
  Card -> m ()
dealCard c = do
  hand <- get
  if evalHand (c:hand) > 21 then do
    throwError (c:hand)
  else do
    liftIO . putStrLn $ show c
    put (c:hand)

loopPlayer :: (Has (State [Card]) sig m, Has (Error [Card]) sig m, MonadIO m) =>
  [Card] -> m ([Card])
loopPlayer (c:cs) = do
  str <- liftIO getLine
  if str == "y" then do
    dealCard c
    loopPlayer cs
  else if str == "n" then do
    return cs
  else do
    loopPlayer (c:cs)

loopDealer :: (Has (State [Card]) sig m, Has (Error [Card]) sig m, MonadIO m) =>
  [Card] -> m ([Card])
loopDealer (c:cs) = do
  hand <- get
  if evalHand hand > 21 then do
    throwError hand
  else if evalHand hand >= 17 then do
    return (c:cs)
  else do
    dealCard c
    loopDealer cs
