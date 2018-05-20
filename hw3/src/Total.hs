module Total where

--------U1
import Control.Exception (throw, Exception(..))

import Control.Monad.Reader
import Control.Monad (liftM2)
import Data.List (find)

import qualified Data.Map.Strict as Map

--------U2
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

--------U3
import Control.Monad.State.Lazy

--------U6
import System.IO

-------U8
import Control.Monad.Cont

--------U10
import System.Environment

------------Unit1

data Expr = Lit Integer | Var String | Expr `Add` Expr | Expr `Sub` Expr | Expr `Mul` Expr | Expr `Div` Expr | Let String Expr Expr
  deriving (Show)

data EvalException = DivisionByZero | DoesntExist String | UnknownError
  deriving (Show)
instance Exception EvalException


-- evalExpr :: [(String, Integer)] -> Expr -> Maybe Integer
-- evalExpr mp expr = runReader (evalRead expr) mp
--   where
--     evalRead :: Expr -> Reader [(String, Integer)] (Maybe Integer)
--     evalRead expr = case expr of
--       Lit val -> return $ return val
--       Var var -> asks (getValue var)
--         where
--           getValue:: String -> [(String,Integer)] -> Maybe Integer
--           getValue str lst = (find (\(a,_)-> a == str) lst) >>= (\(_,b) -> return b)
--       Add e1 e2 -> liftM2 (liftM2 (+)) (evalRead e1) (evalRead e2)
--       Sub e1 e2 -> liftM2 (liftM2 (-)) (evalRead e1) (evalRead e2)
--       Mul e1 e2 -> liftM2 (liftM2 (*)) (evalRead e1) (evalRead e2)
--       Div e1 e2 -> liftM2 (\a b -> (>>=) a divExp <*> b) (evalRead e1) (evalRead e2)
--         where
--           divExp :: Integer -> Maybe (Integer -> Integer)
--           divExp dExp =  if dExp==0
--             then Nothing
--             else return (div `flip` dExp)
--       Let var eIn eOut -> (evalRead eIn) >>= (\val -> case val of
--                                                         Nothing -> return Nothing
--                                                         Just valIn -> local ((:) (var,valIn)) (evalRead eOut))

evalExpr :: (Map.Map String Integer) -> Expr -> Integer
evalExpr mp expr = runReader (evalRead expr) mp
  where
    evalRead :: Expr -> Reader (Map.Map String Integer) Integer
    evalRead e = case e of
      Lit val -> return val
      Var var -> asks (getValue var)
        where
          getValue:: String -> (Map.Map String Integer) -> Integer
          getValue str lst = case Map.lookup str  lst of
            Nothing -> throw (DoesntExist str)
            Just x -> x
      Add e1 e2 -> liftM2 (+) (evalRead e1) (evalRead e2)
      Sub e1 e2 -> liftM2 (-) (evalRead e1) (evalRead e2)
      Mul e1 e2 -> liftM2 (*) (evalRead e1) (evalRead e2)
      Div e1 e2 -> liftM2 (divExp) (evalRead e1) (evalRead e2)
        where
          divExp :: Integer -> Integer -> Integer
          divExp dived divisor =  if divisor == 0
            then throw DivisionByZero
            else dived `div` divisor
      Let var eIn eOut -> (evalRead eIn) >>= (\val -> local (Map.insert var val)  (evalRead eOut))

------------Unit2

type Parser = Parsec Void String
--useful parsers
-- sc :: Parser ()
-- sc = (try (many (satisfy (==' ')) *> return()) )
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

reservedW :: [String]
reservedW = ["let", "in", "=", "mut", ";", "for", "break"]

name :: Parser String
name = (lexeme . try) (p >>= check)
  where
    p :: Parser String
    p = (:) <$> letterChar <*> many alphaNumChar

    check :: String -> Parser String
    check x = if x `elem` reservedW
                  then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                  else return x

--expr parser
exprParser :: Parser Expr
exprParser = makeExprParser term arithOperators

term :: Parser Expr
term = letTerm
   <|> Lit <$> integer
   <|> parens exprParser
   <|> Var <$> name

letTerm :: Parser Expr
letTerm  = do
  rword "(let"
  nameVal <- name
  rword "="
  value <- exprParser
  rword "in"
  exprVal <- exprParser
  rword ")"
  return (nameVal `Let` value $ exprVal)

arithOperators :: [[Operator Parser Expr]]
arithOperators =
  [ [ InfixL (Mul <$ symbol "*")
    , InfixL (Div <$ symbol "/") ]
  , [ InfixL (Add <$ symbol "+")
    , InfixL (Sub <$ symbol "-") ]
  ]

------------Unit3

data EnvironmetException = NoVariableInEnv String | VariableAlreadyExists String
  deriving (Show)
instance Exception EnvironmetException

updV :: (MonadIO m) => String -> Integer -> StateT (Map.Map String Integer) m ()
updV key val = gets (Map.member key) >>= (\found -> if found
                                                    then modify (Map.insert key val)
                                                    else throw (NoVariableInEnv key)
                                          )

defV :: (MonadIO m) => String -> Integer -> StateT (Map.Map String Integer) m ()
defV key val = gets (Map.member key) >>= (\found -> if (not found)
                                                    then modify (Map.insert key val)
                                                    else throw (VariableAlreadyExists key)
                                          )
-------------Unit4
data Stmt = Def String Expr | Upd String Expr
          | Wrt Expr --U6
          | RdV String --U7
          | For String Expr Expr [Stmt] --U8
          | Brk -- U9
  deriving (Show)

semicolon :: Parser ()
semicolon = sc *> try (satisfy (==';')) *> sc

stmtParser :: Parser Stmt
stmtParser = defvParser <* semicolon
         <|> updvParser <* semicolon
         <|> writParser <* semicolon --U6
         <|> readParser <* semicolon --U7
         <|> loopParser

defvParser :: Parser Stmt
defvParser = do
  rword "mut"
  var <- name
  rword "="
  expr <- exprParser
  return (Def var expr)

updvParser :: Parser Stmt
updvParser = do
  var <- name
  rword "="
  expr <- exprParser
  return (Upd var expr)

------------Unit5 + Unit6 + Unit7 + Unit8 + Unit9

evalStmt :: (MonadIO m) => [Stmt] -> (StateT (Map.Map String Integer) m) ()
evalStmt stmts = foldl evalStmtAndAdd (return ()) stmts
  where
    evalStmtAndAdd :: (MonadIO m) => (StateT (Map.Map String Integer) m ()) -> Stmt -> (StateT (Map.Map String Integer) m ())
    evalStmtAndAdd mapStat stmtVal = mapStat >>= (updFun stmtVal)
      where
        updFun :: (MonadIO m) => Stmt -> () ->  StateT (Map.Map String Integer) m ()
        updFun stmt _ = case stmt of
          Def var expr -> do
            env <- get
            defV var (evalExpr env expr)
            return ()
          Upd var expr -> do
            env <- get
            updV var (evalExpr env expr)
            return ()
          Wrt expr -> do
            env <- get
            (liftIO . putStrLn) (show (evalExpr env expr))
            return ()
          RdV var -> do
            str <- (liftIO getLine)
            defV var (read str)
            return ()
          For var begExpr endExpr lst -> do
            env <- get
            defV var (evalExpr env begExpr)
            let bodyEval = evalStmt lst
            let recursive = do
                  env1 <- get
                  let second = (evalExpr env1 endExpr)
                  curVal <- gets (Map.! var)
                  if (curVal >= second)
                    then return ()
                    else do --bodyEval >>= gets (Map.! var) >>= \val -> updV var (val + 1) >> recursive
                        bodyEval
                        val <- gets (Map.! var)
                        updV var (val + 1)
                        recursive
            _ <- recursive
            delV var
            return ()
          Brk -> return ()

------------Unit6

writParser :: Parser Stmt
writParser = do
  rword "<"
  expr <- exprParser
  return (Wrt expr)

------------Unit7

readParser :: Parser Stmt
readParser = do
  rword ">"
  var <- name
  return (RdV var)

------------Unit8

loopParser :: Parser Stmt
loopParser = do
  rword "for"
  Def var begExpr <- defvParser
  semicolon
  endExpr <- exprParser
  rword "{"
  body <- many stmtParser
  rword "}"
  return (For var begExpr endExpr body)

delV :: (MonadIO m) => String -> StateT (Map.Map String Integer) m ()
delV key = gets (Map.member key) >>= (\found -> if found
                                                then modify (Map.delete key)
                                                else throw (NoVariableInEnv key)
                                      )

------------Unit9

brekParser :: Parser Stmt
brekParser = do
  rword "break"
  return Brk

------------Unit10

newtype Code = Code [Stmt]
  deriving (Show)

codeParser :: Parser [Stmt]
codeParser = between sc eof (many stmtParser)

main :: IO ()
main = do
  inputName <- getArgs
  fd <- openFile (head inputName) ReadMode
  content <- hGetContents fd
  let parsed = parse codeParser "" content
  putStrLn $ show content
  putStrLn $ show parsed
  case parsed of
    Right (stmts) -> const () <$> runStateT (evalStmt stmts) Map.empty
    Left _ -> return ()
