module Main where

import Control.Concurrent (forkIO, threadDelay)

import HackSlash

data GameLogic = GameLogic deriving (Show)

mkGameLogic :: GameLogic
mkGameLogic = GameLogic

iter :: GameTime -> DeltaTime -> GameLogic -> HackM GameLogic
iter _gt _dt = return

io :: GameTime -> DeltaTime -> Scene -> GameLogic -> IO GameLogic
io _gt _dt _s g = do
  print g
  return g

main :: IO ()
main = do
  initGame 2016
  _ <- forkIO $ gameLoop 60 iter io mkGameLogic
  threadDelay 1000000
