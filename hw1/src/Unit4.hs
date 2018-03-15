{-# LANGUAGE InstanceSigs #-}
module Unit4 where

--task1
data Pair a = Pair a a
data NonEmpty a = a :| [a]

instance Foldable Pair where
  foldMap :: Monoid m => (a -> m) -> Pair a -> m
  foldMap f (Pair a b) = f a `mappend` f b

  foldr :: (a -> b -> b) -> b -> Pair a -> b
  foldr f z (Pair a b) = a `f` (b `f` z)

instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (a:|as) = f a `mappend` foldMap f as

  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f z (a:|as) = f a (foldr f z as)

--task2
splitOn :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitOn val = foldr (add val) [[]] --list Eta-reduce
  where
    add :: Eq a => a -> a -> [[a]] -> [[a]]
    add value curr ans
        | value == curr = []:ans
        | otherwise = (curr:head ans):drop 1 ans
