{-# LANGUAGE InstanceSigs #-}
module Unit5 where
import Data.Semigroup

--task1
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldr connect [] --mays Eta-reduce
  where
    connect Nothing z = z
    connect (Just vals) z = vals ++ z

--task2
data NonEmpty  a = a :| [a]

instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (a :| as) <> (b :| bs) = a :| (as ++ b : bs)

data ThisOrThat a b = This a | That b | Both a b

instance Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b -- a - самый левый This; b - самый правый That
  (This a) <> (That b) = Both a b
  (This a) <> (This _) = This a
  (This a) <> (Both _ c) = Both a c
  (That _) <> (That b) = That b
  (That a) <> (This b) = Both b a
  (That _) <> (Both b c) = Both b c
  (Both a _) <> (That c) = Both a c
  (Both a b) <> (This _) = Both a b
  (Both a _) <> (Both _ d) = Both a d

--task3
data Builder = One Char | Many [Builder]

instance Semigroup Builder where
  (<>) :: Builder -> Builder -> Builder -- все конкатинируется добавляется слева или справа в зависимости от положения
  (One a) <> (Many []) = One a
  (Many []) <> (One b) = One b
  (One a) <> (One b) = Many [One a, One b]
  (One a) <> (Many bs) = Many (One a : bs)--
  (Many as) <> (One b) = Many (as ++ [One b])
  (Many as) <> (Many bs) = Many (as ++ bs)

instance Monoid Builder where
  mempty :: Builder
  mempty = Many []

  mappend :: Builder -> Builder -> Builder
  mappend (One a) (Many []) = One a
  mappend (Many []) (One b) = One b
  mappend (One a) (One b) = Many [One a, One b]
  mappend (One a) (Many bs) = Many (One a : bs)
  mappend (Many as) (One b) = Many (as ++ [One b])
  mappend (Many as) (Many bs) = Many (as ++ bs)

fromString :: String -> Builder
fromString "" = Many []
fromString chs = Many (map One chs)

toString :: Builder -> String
toString (One ch) = [ch]
toString (Many chs) = concatMap toString chs--concat (map )
