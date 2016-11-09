{-| 'Scaleable' requires that an 'Entity' has a 'Transform' and 'Scaleable' registered to execute. -}

module HackSlash.Component.Scaleable where

import Linear

import HackSlash.Component.Transform
import HackSlash.Game.Time

type ScaleRate = Float

data Scaleable = Scaleable !Scale !ScaleRate deriving (Show)

runScaleable :: DeltaTime -> Maybe Scaleable -> Maybe Transform -> Maybe (Maybe Scaleable, Transform)
runScaleable dt ms@(Just (Scaleable ts sr)) (Just (Transform p r s)) =
  Just (nxt, Transform p r scl)
  where
    scl = sr * dt *^ s
    nxt = if scl >= ts then Nothing else ms
runScaleable _ _ _ = Nothing
