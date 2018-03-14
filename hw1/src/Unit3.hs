{-# LANGUAGE InstanceSigs #-}
module Unit3 where

--task1
data WeekDays = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

nextDay:: WeekDays -> WeekDays
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday
nextDay Sunday = Monday

afterDay:: WeekDays -> WeekDays
afterDay Monday = Sunday
afterDay Tuesday = Monday
afterDay Wednesday = Tuesday
afterDay Thursday = Wednesday
afterDay Friday = Thursday
afterDay Saturday = Friday
afterDay Sunday = Saturday

isWeekend:: WeekDays -> Bool
isWeekend Saturday = True
isWeekend Sunday = True
isWeekend _ = False

daysToParty:: WeekDays -> Int
daysToParty Friday = 0
daysToParty day = (daysToParty (nextDay day)) + 1


--task2
newtype Person = Person String
data Walls = Wood | Stone

data Castle = Castle
  { fortifications  :: Maybe Walls
  , ruler           :: Maybe Person
  }
data Nurture = Library|Church

data House = House
  { family  :: (Person, Maybe Person, Maybe Person, Maybe Person)
  }

countPeople :: House -> Int
countPeople house = countInFamily (family house)
  where
    countInFamily :: (Person, Maybe Person, Maybe Person, Maybe Person) -> Int
    countInFamily (_,b,c,d) = 1 + (exist b) + (exist c) + (exist d)
      where
        exist Nothing = 0
        exist _ = 1

data City = City
  { defence :: Maybe Castle
  , influence :: Maybe Nurture
  , abode  :: (House, [House])
  }

buildCastle :: Castle -> City -> (Bool, City)
buildCastle castle city = searchCastle (defence city)
  where
    searchCastle :: Maybe Castle -> (Bool, City)
    searchCastle Nothing = (True, city {defence = Just castle})
    searchCastle _ = (False, city)


buildNurture :: Nurture -> City -> (Bool, City)
buildNurture n city = searchNurture (influence city)
  where
    searchNurture Nothing = (True, city {influence = Just n})
    searchNurture _ = (False, city)

buildHouse :: (Person, Maybe Person, Maybe Person, Maybe Person) -> City -> City
buildHouse fam city = city {abode = (fst (abode city), House {family = fam}:(snd (abode city)) )}

enterLord :: Person -> City -> Either String City
enterLord lord city = enterDefences (defence city)
  where
    enterDefences Nothing =  Left "no castle in city"
    enterDefences (Just castle) =  searchRuler(ruler castle)
      where
        searchRuler Nothing = Right city {defence = Just (castle {ruler = Just lord})}
        searchRuler _ = Left "city already has lord"

buildWalls :: Walls -> City -> Either String City
buildWalls walls city = searchCastle (defence city)
  where
    searchCastle Nothing = Left "no castle in city"
    searchCastle (Just castle) = searchRuler (ruler castle)
      where
        searchRuler Nothing = Left "city has no lord"
        searchRuler _ =
          let (one,other) = abode city
          in if ((countPeople one) + (sum $ map countPeople other)) < 10
                          then Left "not enougth workers"
                          else Right city {defence = Just (castle {fortifications = Just walls})}


--task3
data Nat = Z | S Nat

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) Z Z = True
  (==) (S z1) (S z2) = z1 == z2
  (==) _ _ = False

instance Ord Nat where
  (<=) :: Nat -> Nat -> Bool
  (<=) (S z1) (S z2) = z1 <= z2
  (<=) Z _ = True
  (<=) _ _ = False

instance Num Nat where
  (+) ::Nat -> Nat -> Nat
  (+) Z b = b
  (+) (S x) b = S (x + b)

  (*) :: Nat -> Nat -> Nat
  (*) Z _ = Z
  (*) (S x) b = b + (x * b)

  (-) :: Nat -> Nat -> Nat
  (-) a Z = a
  (-) a b = (dec a) - (dec b)
    where
      dec Z = Z
      dec (S x) = x

  signum :: Nat -> Nat
  signum _ = S Z

  abs :: Nat -> Nat
  abs a = a

  fromInteger :: Integer -> Nat
  fromInteger z
    |z > 0 = S (fromInteger (z-1))
    |otherwise =  Z


ntoInteger :: Nat -> Int
ntoInteger Z = 0
ntoInteger (S n) = (ntoInteger n) + 1


--task4
data DPTree t = Leaf |  Node
                      { vals    :: [t]
                      , leftCh  :: (DPTree t)
                      , rightCh :: (DPTree t)
                      }

isEmpty :: DPTree t -> Bool
isEmpty Leaf = False
isEmpty _ = True

sizeOf :: DPTree t -> Int
sizeOf Leaf = 0
sizeOf (Node _ l r) = (sizeOf l) + (sizeOf r) + 1

find :: (Ord a) => DPTree a -> a -> Bool
find Leaf _ = False
find (Node values l r) x
  | x == (head values) = True
  | x < (head values) = find l x
  | otherwise = find r x

insert :: (Ord a) => DPTree a -> a -> DPTree a
insert Leaf x = Node [x] Leaf Leaf
insert (Node values l r) x
  | x == (head values) = Node (x:values) l r
  | x < (head values) = Node values (insert l x) r
  | otherwise = Node values l (insert r x)

fromList :: (Ord a) => [a] -> DPTree a
fromList [] = Leaf
fromList (x:xs) = insert (fromList xs) x

remove :: (Ord a) => DPTree a -> a -> DPTree a
remove Leaf _ = Leaf
remove (Node values l r) x
  | x > (head values)= Node values l (remove r x)
  | x < (head values) = Node values (remove l x) r
  | x == (head values) && (drop 1 values) /= [] = Node (drop 1 values) l r
  | otherwise  = delMid l --Want to delete Node
    where
      delMid Leaf = r
      delMid l =
        let maxLVals = findMax l in
        Node maxLVals (remove l (head maxLVals)) r
          where
          findMax :: (Ord a) => DPTree a -> [a]
          findMax (Node vals _ Leaf) = vals
          findMax (Node _ _ r) = findMax r
