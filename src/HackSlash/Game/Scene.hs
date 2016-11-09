module HackSlash.Game.Scene
  ( Scene(..)
  , mkScene
  , mkEntity
  , getTag
  , addTag
  , updateTag
  , deleteTag
  , getTransform
  , addTransform
  , updateTransform
  , deleteTransform
  , getVelocity
  , addVelocity
  , updateVelocity
  , deleteVelocity
  , getCollider
  , addCollider
  , updateCollider
  , deleteCollider
  , getRotatable
  , addRotatable
  , updateRotatable
  , deleteRotatable
  , getScaleable
  , addScaleable
  , updateScaleable
  , deleteScaleable
  , pullEvents
  , iterateScene
  )
  where

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap as IntMap

import HackSlash.Component

import HackSlash.Component.Collider
import HackSlash.Component.Entity
import HackSlash.Component.Tag
import HackSlash.Component.Transform
import HackSlash.Component.Velocity

-- Transform actions
import HackSlash.Component.Moveable
import HackSlash.Component.Rotatable
import HackSlash.Component.Scaleable

import HackSlash.Game.Time

data GameEvent
  = Collision !Entity !Entity
  deriving (Show)

data Scene
  = Scene
  { idGen        :: !Int
  , entityMap    :: !(IntMap Entity)
  , tagMap       :: !(IntMap Tag)
  , transformMap :: !(IntMap Transform)
  , velocityMap  :: !(IntMap Velocity)
  , colliderMap  :: !(IntMap Collider)
  , rotatableMap :: !(IntMap Rotatable)
  , scaleableMap :: !(IntMap Scaleable)
  , gameEventMap :: !(IntMap [GameEvent])
  }

-- | 'mkScene' builds a new 'Scene'.
mkScene :: Scene
mkScene = Scene
  { idGen        = 0
  , entityMap    = IntMap.empty
  , tagMap       = IntMap.empty
  , transformMap = IntMap.empty
  , velocityMap  = IntMap.empty
  , colliderMap  = IntMap.empty
  , rotatableMap = IntMap.empty
  , scaleableMap = IntMap.empty
  , gameEventMap = IntMap.empty
  }

mkEntity :: Scene -> (Entity, Scene)
mkEntity s =
  (nty, s { idGen = eid, entityMap = addMap nty nty (entityMap s) })
  where
    eid = idGen s + 1
    nty = Entity eid

getTag :: Entity -> Scene -> Maybe Tag
getTag e s = lookupMap e (tagMap s)

addTag :: Entity -> Tag -> Scene -> Scene
addTag e a s = s { tagMap = addMap e a (tagMap s) }

updateTag :: Entity -> Tag -> Scene -> Scene
updateTag e a s = s { tagMap = updateMap e (const $ Just a) (tagMap s) }

deleteTag :: Entity -> Scene -> Scene
deleteTag e s = s { tagMap = deleteMap e (tagMap s) }

getTransform :: Entity -> Scene -> Maybe Transform
getTransform e s = lookupMap e (transformMap s)

addTransform :: Entity -> Transform -> Scene -> Scene
addTransform e a s = s { transformMap = addMap e a (transformMap s) }

updateTransform :: Entity -> Transform -> Scene -> Scene
updateTransform e a s = s { transformMap = updateMap e (const $ Just a) (transformMap s) }

deleteTransform :: Entity -> Scene -> Scene
deleteTransform e s = s { transformMap = deleteMap e (transformMap s) }

getVelocity :: Entity -> Scene -> Maybe Velocity
getVelocity e s = lookupMap e (velocityMap s)

addVelocity :: Entity -> Velocity -> Scene -> Scene
addVelocity e a s = s { velocityMap = addMap e a (velocityMap s) }

updateVelocity :: Entity -> Velocity -> Scene -> Scene
updateVelocity e a s = s { velocityMap = updateMap e (const $ Just a) (velocityMap s) }

deleteVelocity :: Entity -> Scene -> Scene
deleteVelocity e s = s { velocityMap = deleteMap e (velocityMap s) }

getCollider :: Entity -> Scene -> Maybe Collider
getCollider e s = lookupMap e (colliderMap s)

addCollider :: Entity -> Collider -> Scene -> Scene
addCollider e a s = s { colliderMap = addMap e a (colliderMap s) }

updateCollider :: Entity -> Collider -> Scene -> Scene
updateCollider e a s = s { colliderMap = updateMap e (const $ Just a) (colliderMap s) }

deleteCollider :: Entity -> Scene -> Scene
deleteCollider e s = s { colliderMap = deleteMap e (colliderMap s) }

getRotatable :: Entity -> Scene -> Maybe Rotatable
getRotatable e s = lookupMap e (rotatableMap s)

addRotatable :: Entity -> Rotatable -> Scene -> Scene
addRotatable e a s = s { rotatableMap = addMap e a (rotatableMap s) }

updateRotatable :: Entity -> Rotatable -> Scene -> Scene
updateRotatable e a s = s { rotatableMap = updateMap e (const $ Just a) (rotatableMap s) }

deleteRotatable :: Entity -> Scene -> Scene
deleteRotatable e s = s { rotatableMap = deleteMap e (rotatableMap s) }

getScaleable :: Entity -> Scene -> Maybe Scaleable
getScaleable e s = lookupMap e (scaleableMap s)

addScaleable :: Entity -> Scaleable -> Scene -> Scene
addScaleable e a s = s { scaleableMap = addMap e a (scaleableMap s) }

updateScaleable :: Entity -> Scaleable -> Scene -> Scene
updateScaleable e a s = s { scaleableMap = updateMap e (const $ Just a) (scaleableMap s) }

deleteScaleable :: Entity -> Scene -> Scene
deleteScaleable e s = s { scaleableMap = deleteMap e (scaleableMap s) }

-- | Simultaneously retrieves and deletes any game events for 'Entity' if they exist.
pullEvents :: Entity -> Scene -> (Maybe [GameEvent], Scene)
pullEvents e s = (lookupMap e (gameEventMap s), s { gameEventMap = deleteMap e (gameEventMap s) })

-- | Iterates the 'Scene' one tick.
iterateScene :: GameTime -> DeltaTime -> Scene -> Scene
iterateScene _gt dt s = cols . movs . scls . rots $ s
  where
    es = entityMap s
    rots s' = foldr rot s' es
    rot e s' =
      case runRotatable dt (lookupMap e (rotatableMap s')) (lookupMap e (transformMap s')) of
        Nothing -> s'
        Just (Just r, t) ->
          s' { rotatableMap = updateMap e (const $ Just r) (rotatableMap s')
             , transformMap = updateMap e (const $ Just t) (transformMap s')
             }
        Just (Nothing, t) ->
          s' { rotatableMap = deleteMap e (rotatableMap s')
             , transformMap = updateMap e (const $ Just t) (transformMap s')
             }
    scls s' = foldr scl s' es
    scl e s' =
      case runScaleable dt (lookupMap e (scaleableMap s')) (lookupMap e (transformMap s')) of
        Nothing -> s'
        Just (Just a, t) ->
          s' { scaleableMap = updateMap e (const $ Just a) (scaleableMap s')
             , transformMap = updateMap e (const $ Just t) (transformMap s')
             }
        Just (Nothing, t) ->
          s' { scaleableMap = deleteMap e (scaleableMap s')
             , transformMap = updateMap e (const $ Just t) (transformMap s')
             }
    movs s' = foldr mov s' es
    mov e s' =
      case runMoveable dt (lookupMap e (velocityMap s')) (lookupMap e (transformMap s')) of
        Nothing -> s'
        Just (v, t) ->
          s' { velocityMap  = updateMap e (const $ Just v) (velocityMap s')
             , transformMap = updateMap e (const $ Just t) (transformMap s')
             }
    cols s' = foldr (\e s'' -> foldr (col e) s'' es) s' es
    col e0 e1 s' =
      case runCollidable a b of
        Nothing -> s'
        Just False -> s'
        Just True ->
          s' { gameEventMap = updateMap e0 (\m -> Just (Collision e0 e1 : m)) .
                              updateMap e1 (\m -> Just (Collision e1 e0 : m)) $ gameEventMap s'
             }
      where
        a = (e0, lookupMap e0 (colliderMap s'), lookupMap e0 (transformMap s'))
        b = (e1, lookupMap e1 (colliderMap s'), lookupMap e1 (transformMap s'))
