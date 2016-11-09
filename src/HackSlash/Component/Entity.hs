{-| An 'Entity' is quite simply an id. -}

module HackSlash.Component.Entity where

newtype Entity = Entity { entity :: Int } deriving (Eq, Show)
