
module Main where

import Game
import List
import Zipper

import Data.List (singleton)
import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, vBorder, hBorder)
import Graphics.Vty

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forever)

import System.Random (StdGen, getStdGen, Random(randomR), random, randoms)

data UI = UI
  { _game  :: Zipper Ball  
  , _load1 :: Ball
  , _load2 :: Ball
  , _gen   :: StdGen
  , _tick  :: Int
  }

listPtrGame :: StdGen -> ListPtr Ball
listPtrGame gen = ListPtr (take 10 (randoms gen)) 0

zipperGame :: StdGen -> Zipper Ball
zipperGame gen = fromList $ take 10 $ randoms gen

initialState :: StdGen -> UI
initialState gen = UI
  { _game  = zipperGame gen''
  , _load1 = load1
  , _load2 = load2
  , _gen   = gen''
  , _tick  = 0
  } 
  where
    (load1, gen')  = random gen
    (load2, gen'') = random gen'

app :: App UI Tick ()
app = App
  { appDraw         = draw
  , appHandleEvent  = handleEvent
  , appStartEvent   = return ()
  , appAttrMap      = const theMap
  , appChooseCursor = neverShowCursor
  }

data Tick = Tick

draw :: UI -> [Widget ()]
draw (UI game l1 l2 _ tick) =
  singleton $
  joinBorders $
  borderWithLabel (str "Zuma") $ 
  vBox $
  [ vLimit 5 (center (str "Use '←' and '→' to control cursor\n↑ to shoot\n↓ to swap ball loaded"))
  , hBorder
  , hBox 
    [ center (str "Left")
    , vBorder
    , hLimitPercent 50 $ vBox
      [ str $ "┏" ++ replicate (2 * trackLen) '━' ++ "┓"
      , hBox $ map (\ball -> withAttr (attrName (show ball)) (str "◢◣")) $ getList game
      , hBox $ map (\ball -> withAttr (attrName (show ball)) (str "◥◤")) $ getList game
      , str $ "┗" ++ replicate (2 * trackLen) '━' ++ "┛"
      , withAttr (attrName (show l1)) $ str $ replicate (2 * (getPtr game)) ' ' ++ "▲"
      , withAttr (attrName (show l2)) $ str $ replicate (2 * (getPtr game)) ' ' ++ "▲"
      ]
    , vBorder
    , center (str $ "Tick: " ++ show tick)
    ]
  , hBorder
  , vLimit 5 (center (str "Bottom"))
  ]

handleEvent :: BrickEvent n Tick -> EventM n UI ()
handleEvent (VtyEvent (EvKey key [])) = do
  UI game l1 l2 gen tick <- get
  case key of
    KChar 'q' -> halt
    KChar 'h' -> put $ UI (moveLeft game) l1 l2 gen tick
    KChar 'l' -> put $ UI (moveRight game) l1 l2 gen tick
    KChar 'k' -> do
      let newGame = shootBall l1 game
      let (newBall, gen') = random gen
      put $ UI newGame l2 newBall gen' tick
    KChar 'j' -> put $ UI game l2 l1 gen tick
    _         -> return ()
handleEvent (AppEvent Tick) = do
  UI game l1 l2 gen tick <- get
  let (newBall, gen') = random gen
  let (newGame, gameOk) = appendBall newBall game
  if gameOk then
    put $ UI newGame l1 l2 gen' (tick + 1)
  else halt
handleEvent _ = return ()


theMap :: AttrMap
theMap = attrMap (white `on` black)
  [ (attrName "R", fg red)
  , (attrName "B", fg blue)
  , (attrName "G", fg green)
  , (attrName "Y", fg yellow)
  , (attrName "W", fg white)
  , (attrName "M", fg magenta)
  ]


main :: IO ()
main = do
  gen <- getStdGen
  let delay = 1500000
  chan <- newBChan 10
  void . forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay delay
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  _ <- customMain initialVty buildVty (Just chan) app (initialState gen)
  putStrLn "GG!"
