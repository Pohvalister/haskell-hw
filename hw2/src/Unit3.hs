{-# LANGUAGE InstanceSigs #-}
module Unit3 where

import Control.Applicative (Alternative(..))
import Data.Bifunctor (first)
import Data.Char (ord)

--task1
data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser parser) = Parser (fmap (first f) . parser)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure val = Parser (\str -> Just (val, str))

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) parFun parVal = Parser ( \str -> case runParser parFun str of
                                    Nothing -> Nothing
                                    Just (rF_F, left_F) -> case runParser parVal left_F of
                                                      Nothing -> Nothing
                                                      Just (rF_V,left_V) -> Just (rF_F rF_V,left_V)
                              )


instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser (const Nothing)--(\_ -> Nothing)

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (<|>) (Parser rF1) (Parser rF2) = Parser (\str -> case rF1 str of
                                              Nothing -> rF2 str
                                              Just (val, left) -> Just (val, left)
                                            )

instance Monad (Parser s) where
  return :: a -> Parser s a
  return val = Parser (\str -> Just (val, str))

  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (>>=) (Parser rFunc) func = Parser (\str -> case rFunc str of
                                        Nothing -> Nothing
                                        Just (val, left) -> runParser (func val) left
                                      )

--task2
ok :: Parser s ()
ok = Parser (\str -> Just ((),str))

isnot :: Parser s a -> Parser s ()
isnot parser = Parser (\s -> case runParser parser s of
  Just _ -> Nothing
  Nothing -> Just ((),s)
                      )
eof :: Parser s ()
eof = Parser (\str -> case str of
  [] -> Just ((),[])
  _ -> Nothing
              )

satisfy :: (s -> Bool) -> Parser s s
satisfy predic = Parser (\str -> case str of
  [] -> Nothing
  x:xs -> if predic x then Just (x,xs) else Nothing
                         )

element :: Eq s => s -> Parser s s
element el = satisfy (el == )

stream :: Eq s => [s] -> Parser s [s]
stream string = Parser (scan [] string)--(\source -> scan [] string source)
  where
    scan :: Eq s => [s] -> [s] -> [s] -> Maybe ([s],[s])
    scan done str src = case (str,src) of
      ([],left) -> Just (done, left)
      (_, []) -> Nothing
      (st:sts, sc:scs) -> if st == sc then scan (done ++ [st]) sts scs else Nothing

--task3
rightBrackets :: Parser Char ()
rightBrackets = (checkBrackets *> eof) <|> eof
  where
    checkBrackets = element '(' *> ((checkBrackets *> element ')') <|> (element ')' *> checkBrackets) <|> element ')')

okC :: Parser Char String
okC = Parser (\str -> Just("", str))

trueBrackets :: Parser Char ()
trueBrackets = checkBrackets *> eof
  where
    checkBrackets = (element '(' *> checkBrackets *> element ')' *> checkBrackets) <|> okC

isDigit::Char -> Bool
isDigit sym = ord sym >= ord '0' && (ord sym <= ord '9')

int :: Parser Char Integer
int =  ((\x -> (-1)*x) <$> (element '-' *> getInt)) <|> (element '+' *> getInt) <|> getInt
  where
    getInt :: Parser Char Integer
    getInt = read <$> some (satisfy isDigit)
