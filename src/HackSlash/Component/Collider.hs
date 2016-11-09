{-| 'Collider' currently uses a very naive AABB collision detection implementation. -}

module HackSlash.Component.Collider where

import Control.Lens
import Linear

import HackSlash.Component.Entity
import HackSlash.Component.Transform

type Radius = Float
type Width  = Float
type Height = Float
type Length = Float

data ColliderType
  = CCylinder
  | CSquare
  | CSphere
  deriving (Eq, Show)

data Collider
  = Collider
  !ColliderType
  {-# UNPACK #-} !Radius
  {-# UNPACK #-} !Width
  {-# UNPACK #-} !Height
  {-# UNPACK #-} !Length
  [Entity]
  deriving (Eq, Show)

mkCollider :: ColliderType -> Radius -> Width -> Height -> Length -> [Entity] -> Collider
mkCollider = Collider

mkCCylinder :: Radius -> Height -> [Entity] -> Collider
mkCCylinder r h es = mkCollider CCylinder r 0.0 h 0.0 es

mkCSquare :: Width -> Height -> Length -> [Entity] -> Collider
mkCSquare w h l es = mkCollider CSquare 0.0 w h l es

mkCSphere :: Radius -> [Entity] -> Collider
mkCSphere r es = mkCollider CSphere r 0.0 0.0 0.0 es

-- Compute the absolute distance and height between two transforms.
tAbs :: Transform -> Transform -> (Float, Float)
tAbs  (Transform pa _ _) (Transform pb _ _) =
  (abs (distance (pa ^. _xz) (pb ^. _xz)), abs ((pa ^. _y) - (pb ^. _y)))

-- | Determines if two 'Collider's are colliding.
isColliding :: (Collider, Transform) -> (Collider, Transform) -> Bool
isColliding (Collider CCylinder ar _ ah _ _, t0) (Collider CCylinder br _ bh _ _, t1) =
  d <= ar + br && h <= ah + bh
  where
    (d, h) = tAbs t0 t1
isColliding (Collider CCylinder _ ar ah _ _, t0) (Collider CSquare _ bw bh bl _, t1) =
  (d <= ar + bw || d <= ar + bl) && h <= ah + bh
  where
    (d, h) = tAbs t0 t1
isColliding (Collider CCylinder ar _ ah _ _, t0) (Collider CSphere br _ _ _ _, t1) =
  d <= ar + br && h <= ah + br
  where
    (d, h) = tAbs t0 t1
isColliding (Collider CSquare _ aw ah al _, t0) (Collider CSquare _ bw bh bl _, t1) =
  (d <= aw + bw || d <= al + bl) && h <= ah + bh
  where
    (d, h) = tAbs t0 t1
isColliding (Collider CSphere ar _ _ _ _, t0) (Collider CSphere br _ _ _ _, t1) =
  d <= ar + br
  where
    (d, _) = tAbs t0 t1
isColliding a b = isColliding b a

isParent :: Entity -> Collider -> Bool
isParent e (Collider _ _ _ _ _ es) = e `elem` es

runCollidable :: (Entity, Maybe Collider, Maybe Transform) -> (Entity, Maybe Collider, Maybe Transform) -> Maybe Bool
runCollidable (e0, Just ca, Just ta) (e1, Just cb, Just tb)
  | ca /= cb && ta /= tb && not (isParent e0 cb) && not (isParent e1 ca) =
      Just $ isColliding (ca, ta) (cb, tb)
  | otherwise = Just False
runCollidable _ _ = Nothing
