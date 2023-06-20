module Main where

import Battle
import Lenses
import MTL (runStateT, runWriterT, StateT, WriterT)
import Data
import Data.List (singleton)

import Data.Functor.Identity
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forever)

import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B

run :: (Battle -> WriterT Logger (StateT Relics Identity) Battle) -> UI -> UI
run k (UI b s w c e i v) =
  let ((b1, w1), s1) = runIdentity . flip runStateT s . runWriterT $ k b in
    UI b1 s1 (w1 `mappend` w) c e i v

initialUI :: UI
initialUI = UI
  { _battle = initialBattle
  , _state = Relics
    { _suriken = 0
    , _kunai   = 0
    }
  , _writer = Logger
    { _history = ""
    , _cardsPlayed = Sum 0
    }
  , _cIndex = 0
  , _eIndex = 0
  , _tick   = 0
  , _mode   = NormalMode
  }

data UI = UI
  { _battle :: Battle
  , _state  :: Relics
  , _writer :: Logger
  , _cIndex :: Int
  , _eIndex :: Int
  , _tick   :: Int
  , _mode   :: ViewMode
 }

data ViewMode = NormalMode | DrawPileMode | DiscardPileMode | LoggerMode
  deriving (Eq)

data Tick = Tick

battle :: Lens UI Battle
battle f b = fmap (\a -> b { _battle = a }) $ f (_battle b)

cIndex :: Lens UI Int
cIndex f b = fmap (\a -> b { _cIndex = a }) $ f (_cIndex b)

eIndex :: Lens UI Int
eIndex f b = fmap (\a -> b { _eIndex = a }) $ f (_eIndex b)

tick :: Lens UI Int
tick f b = fmap (\a -> b { _tick = a }) $ f (_tick b)

state :: Lens UI Relics
state f b = fmap (\a -> b { _state = a }) $ f (_state b)

writer :: Lens UI Logger
writer f b = fmap (\a -> b { _writer = a }) $ f (_writer b)

mode :: Lens UI ViewMode
mode f b = fmap (\a -> b { _mode = a }) $ f (_mode b)

app :: App UI Tick ()
app = App
  { appDraw         = draw
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = startEvent
  , appAttrMap      = const theMap
  }


draw :: UI -> [Widget ()]
draw ui = singleton $ B.border $ vBox [header, drawRelics (ui ^. state), B.hBorder, body (ui ^. mode)]
  where
    body :: ViewMode -> Widget ()
    body NormalMode = drawBattle $ ui ^. battle
    body DrawPileMode = 
      (C.center $ hBox $ zipWith drawCard (repeat False) (ui ^. battle . cardPool . drawPile))
    body DiscardPileMode = 
      (C.center $ hBox $ zipWith drawCard (repeat False) (ui ^. battle . cardPool . discardPile))
    body LoggerMode = 
      (str $ show $ _cardsPlayed $ ui ^. writer)
      <=>
      (strWrap $ _history $ ui ^. writer)

    drawRelics :: Relics -> Widget ()
    drawRelics relics_ = vLimit 3 $ hBox $
      [ str $ "suriken(" ++ show (relics_ ^. suriken) ++ ")"
      , str $ "kunai(" ++ show (relics_ ^. kunai) ++ ")"
      ]

    drawBattle :: Battle -> Widget ()
    drawBattle b =
      (vLimitPercent 50 $ padAll 0 $ 
        (drawUnit False (b ^. player)
        <+>
        (vBox $ zipWith drawUnit (boolEncode $ ui ^. eIndex) (b ^. enemies)))
      )
      <=>
      (
        (padRight Max $ padTop Max $
          (
            (B.border $ str $ "Energy[" ++ (show $ b ^. mana) ++ "]")
            <=>
            (B.border $ str $ "Pile[" ++ (show $ length $ b ^. cardPool . drawPile) ++ "]")
          )
        )
        <+>
        (padTop Max $ hBox $ zipWith drawCard (boolEncode $ ui ^. cIndex) (b ^. cardPool . hand))
        <+>
        (padLeft Max $ padTop Max $
          (
            (B.border $ str $ "End Turn")
            <=>
            (B.border $ str $ "DiscardPile[" ++ (show $ length $ b ^. cardPool . discardPile) ++ "]")
          )
        )
      )

    drawUnit :: Bool -> Unit -> Widget ()
    drawUnit pointer u = C.center $ vLimit 4 $
      (C.center $ str $ show $ u ^. intends $ ui ^. battle . turn)
      <=>
      (C.center $ str $ show (u ^. name) ++ ": " ++ show (u ^. health) ++ "/" ++ show (u ^. maxHealth)
        ++ if u ^. block > 0 then "[" ++ show (u ^. block) ++ "]" else ""
      )
      <=>
      (C.center $ str $ "(^･ｪ･^)" ++ if pointer then "   ⬅" else "")
      <=>
      (C.center $ str $ concat $ zipWith (\a v -> if v == 0 then "" else a ++ show v ++ " ")
        ["s"          , "d"           , "w"      , "v"            , "f"       ]
        [u ^. strength, u ^. dexterity, u ^. weak, u ^. vulnerable, u ^. frail]
      )

    drawCard :: Bool -> Card -> Widget ()
    drawCard pointer_ card_ = vLimit 14 $ hLimit 11 $
      (vLimitPercent 70 $ B.borderWithLabel
        (str $ "[" ++ show (card_ ^. cardCost) ++ "]" ++ show (card_ ^. cardName))
        (padBottom Max $ (strWrap $ card_ ^. cardInfo))
      )
      <=>
      (C.center $ str $ if pointer_ then "⬆" else "")

    header :: Widget ()
    header = vLimit 3 $ hBox $
      [ (padRight Max $ B.border $ str $ show $ ui ^. battle . player . name)
      , (C.center $ str $ "Slay the Spire")
      , (padLeft Max $ B.border $ str $ "Turn: " ++ show (ui ^. battle . turn))
      ]

handleEvent :: BrickEvent () Tick -> EventM () UI ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey key  [])) = do
  ui <- get
  if ui ^. tick /= 0
    then return ()
  else case key of
    V.KChar 'l' -> put $ ui & cIndex %~ incUpTo (length (ui ^. battle . cardPool . hand) - 1)
    V.KChar 'h' -> put $ ui & cIndex %~ dec 
    V.KChar 'k' -> put $ ui & eIndex %~ dec
    V.KChar 'j' -> put $ ui & eIndex %~ incUpTo (length (ui ^. battle . enemies) - 1)
    V.KEnter    -> put $ ui & run (playCardOnEnemy (ui ^. cIndex) (ui ^. eIndex))
    V.KChar 'e' -> put $ (ui & run endTurn) & tick %~ (+1)
    V.KChar 'n' -> put $ ui & mode .~ NormalMode
    V.KChar 'v' -> put $ ui & mode .~ LoggerMode
    V.KChar 'p' -> put $ ui & mode .~ DrawPileMode
    V.KChar 'd' -> put $ ui & mode .~ DiscardPileMode
    _         -> return ()
handleEvent (AppEvent Tick) = do
  ui <- get
  baseOnTick (ui ^. tick)
    where
      baseOnTick :: Int -> EventM () UI ()
      baseOnTick 0 = return ()
      baseOnTick 1 = get >>= \ui -> put $ (run startETurn ui) & tick %~ (+1)
      baseOnTick 2 = get >>= \ui -> put $ (run endETurn ui) & tick %~ (+1)
      baseOnTick 3 = do
        ui <- get
        let ui1 = ui  & run startTurn
        let ui2 = ui1 & cIndex .~ 0
        let ui3 = ui2 & eIndex .~ 0
        put $ ui3 & tick .~ 0
      baseOnTick _ = return ()
handleEvent _ = return ()

startEvent :: EventM () UI ()
startEvent = get >>= put . run startBattle

theMap :: AttrMap
theMap = attrMap (V.white `on` V.black) []

-----------------------------------------------------------------------------

boolEncode :: Int -> [Bool]
boolEncode 0 = True : repeat False
boolEncode n = False : boolEncode (n-1) 

incUpTo :: Int -> Int -> Int
incUpTo top n
  | top == n  = n
  | otherwise = n + 1

dec :: Int -> Int
dec 0 = 0
dec n = n - 1



main :: IO ()
main = do
  let delay = 1000000
  chan <- newBChan 10
  void . forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay delay
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  _ <- customMain initialVty buildVty (Just chan) app initialUI
  putStrLn "GG!"
