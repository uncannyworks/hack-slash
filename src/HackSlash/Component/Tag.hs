{-| A 'Tag' allows an 'Entity' to be identified by a string.
    Multiple entities can be tagged and found with the same string.
 -}

module HackSlash.Component.Tag where

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.Text (Text)

import HackSlash.Component.Entity

newtype Tag = Tag { tag :: Text } deriving (Eq, Show)

-- | Find all 'Entity' tagged with 'Tag'.
findTagged :: Tag -> IntMap Tag -> [Entity]
findTagged t m = fmap Entity (IntMap.keys . IntMap.filter (== t) $ m)

-- | Convenience method.
findTaggedString :: Text -> IntMap Tag -> [Entity]
findTaggedString t = findTagged (Tag t)
