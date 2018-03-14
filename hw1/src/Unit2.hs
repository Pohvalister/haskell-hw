module Unit2 where

--task1
eraseElement :: Int -> [a] -> (a, [a])
eraseElement n list =
  let (fstH,sndH) = splitAt n list in
  (head sndH , fstH ++ (drop 1 sndH))

--task2
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [val] = [val]
mergeSort vals =
  --splitAt (div (length xs) 2) xs
  let (fstH, sndH) = splitAt `flip` vals $ div `flip` 2 $ length vals in
  merge (mergeSort fstH) $ mergeSort sndH
  where
    merge :: Ord a => [a]->[a]->[a]
    merge xs [] = xs
    merge [] ys = ys
    merge (x:xs) (y:ys)
      | x < y = x:(merge xs (y:ys))
      | otherwise = y:(merge (x:xs) ys)
