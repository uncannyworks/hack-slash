module HackSlash.Component where

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

import HackSlash.Component.Entity

addMap :: Entity -> a -> IntMap a -> IntMap a
addMap (Entity eid) = IntMap.insert eid

lookupMap :: Entity -> IntMap a -> Maybe a
lookupMap (Entity eid) = IntMap.lookup eid

updateMap :: Entity -> (a -> Maybe a) -> IntMap a -> IntMap a
updateMap (Entity eid) s = IntMap.update s eid

deleteMap :: Entity -> IntMap a -> IntMap a
deleteMap (Entity eid) = IntMap.delete eid
