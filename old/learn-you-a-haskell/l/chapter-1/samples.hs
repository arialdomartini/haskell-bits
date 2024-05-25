module M where

cons :: (Ord a) => a -> [a] -> [a]
cons x xs = x : xs
