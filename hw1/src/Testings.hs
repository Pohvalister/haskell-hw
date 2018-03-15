{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Testings where

import System.Random (newStdGen, randomRs)

import Unit1
import Unit2
import Unit3
import Unit4
import Unit5
--foldr (&&) True
--runTests f tests = all (\test -> f (fst test) == (snd test)) tests -- and (map ) --
runTests f = all (\ test -> f (fst test) == (snd test))

-- block1
order3IntT =  [((2,3,1),(1,2,3)),((-10, -10, 0)
              , (-10, -10, 0)),((2, -1, 3),(-1,2,3))]
order3CharT = [(('b','c','a'),('a','b','c'))]

testUnit1_1 = runTests Unit1.order3 order3IntT
testUnit1_2 = runTests Unit1.order3 order3CharT

smartReplicateT = [([1,2,3],[1,2,2,3,3,3]),([],[]),([3,2,1],[3,3,3,2,2,1]),([0],[]),([2],[2,2])]
testUnit1_3 = runTests Unit1.smartReplicate smartReplicateT
containsT = [[1 .. 5], [2,0],[3,4]]
containsV = 3
testUnit1_4 = show containsT ++ " " ++ show containsV ++ " | " ++ show (Unit1.contains containsV containsT)

stringSumT = [ ("1",1), ("1 2 3",6), (" 1",1), ("1 ",1), ("\t1\t",1), ("\t12345\t",12345)
            , ("010 020 030",60), (" 123 456 789 ",1368), ("-1",-1), ("-1 -2 -3",-6)
            , ("\t-12345\t",-12345), (" -123 -456 -789 ",-1368)
            , ("\n1\t\n3   555  -1\n\n\n-5",553), ("123\t\n\t\n\t\n321 -4 -40",400)
            ]
testUnit1_5 = runTests Unit1.stringSum stringSumT

--block3
house = House (Person "qw", Just $ Person "e", Just $ Person "qwe",Nothing)

checkCity answer = case answer of
  (Left str) -> show str
  (Right _)  -> show "good"

castle = Castle
   { fortifications = Just Wood
   , ruler = Nothing
   }
city = City
   { defence = Just castle
   , influence = Nothing
   , abode = (house,[])
}
castle1 = Castle
   { fortifications = Just Wood
   , ruler = Just (Person "s")
   }
city1 = City
  { defence = Just castle1
  , influence = Nothing
  , abode = (house,[])
  }
city2 = City
  { defence = Just castle1
  , influence = Nothing
  , abode = (house,[house,house,house,house])
}

answer1 = buildWalls Wood city
answer2 = enterLord (Person "s") city
answer3 = buildWalls Wood city1
answer4 = buildWalls Stone city2


randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen


testUnit3_4 =
  let test0 = insert Leaf 6
  in let test1 = insert test0 1
     in let test2 = insert test1 6
        in let test3 = insert test2 10
           in let test4 = remove test3 6
              in find test4 6
testUnit3_4_2 =
  let test0 = fromList [2,3,4,1,5,5,67,6,43,45,-1,3,4]
  in let test1 = remove test0 3
     in let test2 = remove test1 3
         in let test3 = remove test2 3
            in let test4 = remove test3 3
               in let test5 = remove test4 67
                  in find test5 3

--block5
testUnit5_3 = show (Unit5.toString (Unit5.fromString "some text"))
