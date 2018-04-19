{-# LANGUAGE InstanceSigs #-}
module Unit2 where

import Text.Read (readMaybe)
import Control.Applicative (liftA2)

--task1
stringSum :: String -> Maybe Int
stringSum str = foldr (liftA2 (+)) (pure 0) ([readMaybe] <*> words str)

--task2
data Optional a = Optional (Maybe (Maybe a))

instance Monad Optional where
  return :: a -> Optional a
  return val = Optional (Just (Just val))

  (>>=) :: Optional a -> (a -> Optional b) -> Optional b
  (>>=) val func = case val of
    Optional Nothing -> Optional Nothing
    Optional (Just Nothing) -> Optional (Just Nothing)
    Optional (Just (Just x)) -> func x

instance Functor Optional where
  fmap :: (a -> b) -> Optional a -> Optional b
  fmap func val = case val of
    Optional Nothing -> Optional Nothing
    Optional (Just Nothing) -> Optional (Just Nothing)
    Optional (Just (Just x)) -> Optional (Just (Just (func x)))
  (<$) :: a -> Optional b -> Optional a
  (<$) val opt = case opt of
    Optional Nothing -> Optional Nothing
    Optional (Just Nothing) -> Optional (Just Nothing)
    Optional (Just (Just _)) -> Optional (Just (Just val))

instance Applicative Optional where -- Functor Optional =>
  pure :: a -> Optional a
  pure = Optional . Just . Just

  (<*>) :: Optional (a -> b) -> Optional a -> Optional b
  (<*>) optF optV = case optF of
    Optional Nothing -> Optional Nothing
    Optional (Just Nothing) -> Optional(Just Nothing)
    Optional (Just (Just func)) -> fmap func optV

instance Foldable Optional where
  foldr :: (a -> b -> b) -> b -> Optional a -> b
  foldr _ z (Optional Nothing) = z
  foldr _ z (Optional (Just Nothing)) = z
  foldr f z (Optional (Just (Just val))) = f val z

instance Traversable Optional where --  (Functor t, Foldable t) =>
  traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
  traverse func opt = case opt of
    Optional Nothing -> pure (Optional Nothing)
    Optional (Just Nothing) -> pure (Optional(Just Nothing))
    Optional (Just (Just x)) -> Optional . Just . Just <$> func x
  sequenceA :: Applicative f => Optional (f a) -> f (Optional a)
  sequenceA optD = case optD of
    Optional Nothing -> pure (Optional Nothing)
    Optional (Just Nothing) -> pure (Optional (Just Nothing))
    Optional (Just (Just val)) -> pure (Optional . Just . Just) <*> val

--task3
