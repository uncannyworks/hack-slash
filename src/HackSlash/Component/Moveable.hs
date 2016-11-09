{-| 'Moveable' requires that an 'Entity' have a 'Transform' and 'Velocity' registered to execute. -}

module HackSlash.Component.Moveable where

import Linear

import HackSlash.Component.Transform
import HackSlash.Component.Velocity
import HackSlash.Game.Time

runMoveable :: DeltaTime -> Maybe Velocity -> Maybe Transform -> Maybe (Velocity, Transform)
runMoveable dt (Just (Velocity d cv tv mv a)) (Just (Transform p r s)) =
  Just (Velocity d vel tv mv a, Transform pos r s)
  where
    vel = (\v -> if v >= mv then mv else v) (tv + a * dt)
    pos = p + (cv * dt *^ rotate r d)
runMoveable _ _ _ = Nothing
