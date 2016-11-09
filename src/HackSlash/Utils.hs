{-| Utils simply contains convenience functions. -}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module HackSlash.Utils where

toRadians :: Floating x => x -> x
toRadians x = x * pi / 180

toDegrees :: Floating x => x -> x
toDegrees x = x * 180 / pi

isPositive :: (Num a, Ord a) => a -> Bool
isPositive = (>=) 0

isNegative :: (Num a, Ord a) => a -> Bool
isNegative = (<=) 0

{-| 'Range' allows a numeric range to be specified and tested against.

Explicitly between the range:

@9 `'between'` 'Range' -10 10@

Favor the left:

@-10 `'betweenl'` 'Range' -10 10@

Favor the right:

@10 `'betweenr'` 'Range' -10 10@

Favor either side:

@10 `'betweenBoth'` 'Range' -10 10@
-}

data Range a = Range a a

between :: Ord a => a -> Range a -> Bool
between a (Range b c) = a > b && a < c

betweenl :: Ord a => a -> Range a -> Bool
betweenl a (Range b c) = a >= b && a < c

betweenr :: Ord a => a -> Range a -> Bool
betweenr a (Range b c) = a > b && a <= c

betweenBoth :: Ord a => a -> Range a -> Bool
betweenBoth a (Range b c) = a >= b && a <= c
