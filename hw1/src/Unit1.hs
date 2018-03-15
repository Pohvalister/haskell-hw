module Unit1 where

--check
inc :: Int -> Int
inc x = x + 1

--task1
order3 :: Ord a => (a, a, a)->(a, a, a)
order3 (a,b,c)
    | a < b = order3_1 (a,b,c)
    | otherwise = order3_1 (b,a,c)
  where
    order3_1 (a1,b1,c1)
        | b1 < c1 = order3_2 (a1,b1,c1)
        | otherwise = order3_2 (a1,c1,b1)
      where
        order3_2 (a2,b2,c2)
            | a2 < b2 = (a2,b2,c2)
            | otherwise = (b2,a2,c2)

--task2
smartReplicate :: [Int] -> [Int]
smartReplicate a
    | null a = []--a == []
    | otherwise = replicate (head a) (head a) ++ smartReplicate (drop 1 a)

--task3
contains ::  Eq a => a -> [[a]] -> [[a]]
contains val = filter (elem val) --lists

--task4
stringSum :: String -> Int
stringSum str = sum $ map read $ words str
