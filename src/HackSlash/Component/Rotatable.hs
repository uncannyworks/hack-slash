{-| 'Rotatable' requires that an 'Entity' has a 'Transform' and 'Rotatable' registered to execute. -}

module HackSlash.Component.Rotatable where

import Linear

import HackSlash.Component.Transform
import HackSlash.Game.Time

type RotationRate = Float

data Rotatable = Rotatable !Rotation !RotationRate deriving (Show)

runRotatable :: DeltaTime -> Maybe Rotatable -> Maybe Transform -> Maybe (Maybe Rotatable, Transform)
runRotatable dt mr@(Just (Rotatable tr rr)) (Just (Transform p r c)) =
  Just (nxt, Transform p rot c)
  where
    rot = slerp r (r * tr) (dt * rr)
    nxt = if rot == tr then Nothing else mr
runRotatable _ _ _ = Nothing
