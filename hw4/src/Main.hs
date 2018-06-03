{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

--------TemplateHaskell

import Language.Haskell.TH

import Shew
--U1--
chooseByIndices :: Int -> [Int] -> Q Exp
chooseByIndices n lst = do let names = map (\x -> varE (mkName ("x_" ++ show x))) lst
                           lamE [tupP (map (\x -> varP (mkName ("x_" ++ show x))) [0..n-1])] (tupE names)

--U2--
data SomeData = SomeData { some :: String }
deriveTextDef ''SomeData

data ShowData = ShowData { show :: String }
  deriving Show
deriveText ''ShowData

--------Lens
--U1--
newtype Const a x = Const {getConst :: a}
instance Functor (Const a) where
  fmap _ (Const v) = Const v

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

--view :: ((a -> Const a a) -> s -> Const a s) -> s -> a
view :: Lens' s a -> s -> a
view lns s = getConst (lns Const s)

newtype Identity a = Identity {runIdentity :: a}
instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

--over :: ((a -> Identity a) -> s -> Identity s) -> (a -> a) -> s -> s
over :: Lens' s a -> (a -> a) -> s -> s
over lns fn s = runIdentity (lns (Identity . fn) s)

--set :: ((a -> Identity a) -> s -> Identity s) -> a -> s -> s
set :: Lens' s a -> a -> s -> s
set lns a s = runIdentity (lns (Identity . const a) s)

(.~) :: Lens' s a -> a -> s -> s
(.~) = set

(^.) :: s -> Lens' s a -> a
s ^. lns = view lns s

(%~) :: Lens' s a -> (a -> a) -> s -> s
(%~) = over

---------
-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 fn (a, x) = (\val -> (val, x)) <$> fn a

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 fn (x, a) = (\val -> (x, val)) <$> fn a --(,) x <$> fn a

---------
-- lens' :: (s -> a) -> (s -> a -> s) -> Lens' s a
-- lens' getter setter = \fn from -> (setter from) <$> (fn (getter from))

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter fn from = setter from <$> fn (getter from)

-- Объединить две линзы в одну, которая работает с Either.
choosing :: Lens s1 t1 a b -> Lens s2 t2 a b -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 fn from = case from of
                              Left frL -> Left <$> l1 fn frL
                              Right frR -> Right <$> l2 fn frR

--eq :: Lens s t a b -> (a -> b) -> s -> t

-- Изменить цель линзы и вернуть новый результат. Постарайтесь
-- реализовать без анонимных функций и определений своих функций
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = (l ((\t -> (t, t)) . f)) s

-- Изменить цель линзы, но вернуть старый результат.
--(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
--(<<%~) l f s = ((l (\t -> (t, t))) (\fun (a, b) -> (a, f b) f) s

--U2--

data FS
    = Dir
          { name     :: FilePath  -- название папки, не полный путь
          , contents :: [FS]
          }
    | File
          { name     :: FilePath  -- название файла, не полный путь
          }
