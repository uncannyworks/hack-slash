{-# LANGUAGE BangPatterns #-}

module HackSlash.Game
  ( FixedRate
  , HackM
  , initGame
  , gameIterate
  , gameLoop
  -- re-export
  , get
  , put
  )
  where

import Control.Concurrent (threadDelay)
import Control.Monad.State.Lazy
import System.Clock
import System.Random

import HackSlash.Game.Scene
import HackSlash.Game.Time

type FixedRate = Float

{-| Hack State Monad

    This is simply a type synonymn which hides the State Scene bits.
-}

type HackM a = State Scene a

{-| Initialize the game.

    Initializes the random number generator with the supplied 'Int' seed.
-}
initGame :: Int -> IO ()
initGame = setStdGen . mkStdGen

{-| Iterate the game update function one tick forward.

    This can be useful to call for games which are turn based and don't
    require a full blown game loop to operate.
-}
gameIterate :: GameTime                                -- ^ Total elapsed game time
            -> DeltaTime                               -- ^ Delta time
            -> g                                       -- ^ Game specific data
            -> Scene                                   -- ^ Internal 'Scene' data
            -> (GameTime -> DeltaTime -> g -> HackM g) -- ^ Game specific update function
            -> (g, Scene)
gameIterate gt dt g s f = runState (f gt dt g) (iterateScene gt dt s)

{-| Takes the fixed rate to run at, user defined game data, and a function
    which processes the game data and can manipulate the engine components.

    This is currently a fixed time step function.
-}
gameLoop :: FixedRate                                     -- ^ Fixed rate at which to tick the game loop
         -> (GameTime -> DeltaTime -> g -> HackM g)       -- ^ Game specific update function
         -> (GameTime -> DeltaTime -> Scene -> g -> IO g) -- ^ Game specific IO update function
         -> g                                             -- ^ Game specific data
         -> IO ()                                         -- ^ Execute in the IO monad
gameLoop fr f fio g = loop 0.0 g mkScene
  where
    dt = 1.0 / fr
    dt' = floor (dt * 1000) -- frame rate in microseconds
    loop !gt g0 s0 = do
      -- time snapshot before update
      t0 <- getTime Monotonic

      let (g1, s1) = gameIterate gt dt g0 s0 f -- game update
      g2 <- fio gt dt s1 g1                    -- IO update

      -- time snapshot after update
      t1 <- getTime Monotonic

      -- difference in microseconds
      let d' = fromIntegral $ toNanoSecs (diffTimeSpec t0 t1) `div` 1000

      -- pause thread for difference
      threadDelay (dt' - d')

      -- loop, increment total elapsed game time
      loop (gt + fr) g2 s1
