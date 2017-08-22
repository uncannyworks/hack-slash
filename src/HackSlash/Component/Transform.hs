{-| 'Transform' is a standard data type which describes the position,
    rotation, and scale of an object in the scene.

    Behind the scenes it uses the "linear" library by Edward Kmett.

    'Position' and 'Scale' are simply type synonyms for 'V3 Float',
    and 'Rotation' a type synonym for 'Quaternion Float'.
-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module HackSlash.Component.Transform where

import Linear.V3
    ( V3(..)
    , cross
    )
import Linear.Quaternion
    ( Quaternion(..)
    , axisAngle
    , slerp
    , rotate
    )
import Linear.Metric
    ( dot
    , normalize
    )

import HackSlash.Game.Time

type Position = V3 Float

mkPosition :: Float -> Float -> Float -> Position
mkPosition = V3

idPosition :: Position
idPosition = mkPosition 0.0 0.0 0.0

positionToTuple :: Position -> (Float, Float, Float)
positionToTuple (V3 x y z) = (x, y, z)

-- | Direction is simply a type synonym for normalized vectors which point in a
--   specific direction.
type Direction = Position

-- | Forward along the Z axis.
forwardV :: Direction
forwardV = mkPosition 0.0 0.0 1.0

-- | Backward along the Z axis.
backwardV :: Direction
backwardV = mkPosition 0.0 0.0 (-1.0)

-- | Up along the Y axis.
upV :: Direction
upV = mkPosition 0.0 1.0 0.0

-- | Down along the Y axis.
downV :: Direction
downV = mkPosition 0.0 (-1.0) 0.0

-- | Right along the X axis.
rightV :: Direction
rightV = mkPosition 1.0 0.0 0.0

-- | Left along the X axis.
leftV :: Direction
leftV = mkPosition (-1.0) 0.0 0.0

type Rotation = Quaternion Float

mkRotation :: Float -> Float -> Float -> Float -> Rotation
mkRotation x y z w = Quaternion w (V3 x y z)

idRotation :: Rotation
idRotation = mkRotation 0.0 0.0 0.0 1.0

rotationToTuple :: Rotation -> (Float, Float, Float, Float)
rotationToTuple (Quaternion w (V3 x y z)) = (x, y, z, w)

doRotate :: DeltaTime -> Float -> Rotation -> Rotation -> Rotation
doRotate dt rate tar rot = slerp rot (rot * tar) (dt * rate)

type Scale = V3 Float

mkScale :: Float -> Float -> Float -> Scale
mkScale = V3

idScale :: Scale
idScale = mkScale 1.0 1.0 1.0

scaleToTuple :: Scale -> (Float, Float, Float)
scaleToTuple (V3 x y z) = (x, y, z)

data Transform = Transform !Position !Rotation !Scale deriving (Eq, Show)

mkTransform :: Position -> Rotation -> Scale -> Transform
mkTransform = Transform

idTransform :: Transform
idTransform = mkTransform idPosition idRotation idScale

-- Utils

-- | Returns the angle in radians between the forward facing position a and the
--   target b.
getTargetAngle :: Position -> Position -> Rotation -> Float
getTargetAngle a b r =
  acos (rot `dot` nrm)
  where
    rot = rotate r forwardV
    nrm = normalize (b - a)

rotateToTarget :: Position -> Transform -> Transform
rotateToTarget tarPos (Transform pos rot scl) =
  Transform pos (axisAngle axis' angle') scl
  where
    curHeading = rotate rot forwardV
    tarDir     = normalize (tarPos - pos)
    angle'     = acos $ curHeading `dot` tarDir
    axis'      = curHeading `cross` tarDir
