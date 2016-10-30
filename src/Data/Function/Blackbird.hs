module Data.Function.Blackbird ((...), blackbird) where

(...), blackbird :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...)     = blackbird
blackbird = (.) . (.)

infixr 8 ...
