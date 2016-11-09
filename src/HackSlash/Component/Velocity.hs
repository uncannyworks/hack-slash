{-| 'Velocity' defines how an 'Entity' with a 'Transform' moves. -}

module HackSlash.Component.Velocity where

import HackSlash.Component.Transform (Direction, forwardV)

type Acceleration = Float
-- Velocity
type CurrentV     = Float
type TargetV      = Float
type MaxV         = Float

data Velocity
  = Velocity
    !Direction
    {-# UNPACK #-} !CurrentV
    {-# UNPACK #-} !TargetV
    {-# UNPACK #-} !MaxV
    {-# UNPACK #-} !Acceleration
  deriving (Show)

mkVelocityZero :: Velocity
mkVelocityZero = Velocity forwardV 0.0 0.0 0.0 0.0

mkVelocity :: Direction -> CurrentV -> TargetV -> MaxV -> Acceleration -> Velocity
mkVelocity = Velocity

setVDirection :: Direction -> Velocity -> Velocity
setVDirection d (Velocity _ f0 f1 f2 f3) = Velocity d f0 f1 f2 f3

setVTarget :: TargetV -> Velocity -> Velocity
setVTarget t (Velocity f0 _ f1 f2 f3) = Velocity f0 t f1 f2 f3
