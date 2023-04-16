module Util
  ( decompose
  , add
  , sub
  )
  where

decompose :: [a] -> [(a, [a])]
decompose = \case
  [] -> []
  x : xs -> (x, xs) : decompose xs

add :: Num a => a -> a -> a
add = (+)

sub :: Num a => a -> a -> a
sub = (-)
