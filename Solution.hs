module Solution where
import Data.Char
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec as Parsec (many)

data Expr
  = Var String
  | Val Number
  | Oper OperType Expr Expr
  | Function FunType Expr
  deriving (Show)

data OperType
  = Add | Multiply | Subtract | Divide | Power
  deriving (Show)
  
data Number
  = IntegerNum Integer
  | DecimalNum Double
  deriving (Show)
  
data FunType
  = Derivative | Integration | Sin | Cos | Tan | CoTan
  deriving (Show)

eval :: Expr -> Maybe Double
eval (Var _) = Nothing
eval (Val n) = evalNumber n
eval (Oper op e1 e2) = applyOperation op (eval e1) (eval e2)
eval (Function fT e1) = applyFunction fT (eval e1)

applyFunction :: FunType -> Maybe Double -> Maybe Double
applyFunction fT (Just x) =
  case fT of
    Derivative -> Just x
    Integration -> Just 0 

evalNumber :: Number -> Maybe Double
evalNumber (IntegerNum x) = Just (fromIntegral x)
evalNumber (DecimalNum x) = Just x

applyOperation :: OperType -> Maybe Double -> Maybe Double -> Maybe Double
applyOperation op (Just x) (Just y) =
  case op of
    Add -> Just (x + y)
    Multiply -> Just (x * y)
    Subtract -> Just (x - y)
    Divide -> if y /= 0 then Just (x / y) else Nothing
    Power -> Just (x ** y)
applyOperation op _ _ = Nothing

evalOperation :: OperType -> Either Double Expr -> Either Double Expr -> Expr
evalOperation Add (Left x) (Left y) = Val (DecimalNum (x + y))
evalOperation Multiply (Left x) (Left y) = Val (DecimalNum (x * y))
evalOperation Subtract (Left x) (Left y) = Val (DecimalNum (x - y))
evalOperation Divide (Left x) (Left y)
  | y /= 0 = Val (DecimalNum (x / y))
  | otherwise = Val (DecimalNum 0) 
evalOperation Power (Left x) (Left y) = Val (DecimalNum (x ** y))

partialEval :: Expr -> Expr
partialEval (Var x) = Var x
partialEval (Val n) = Val n
partialEval (Oper op e1 e2) =
  case (eval e1, eval e2) of
    (Just x, Just y) ->
      case applyOperation op (Just x) (Just y) of
        Just result -> Val (DecimalNum result)
    _ -> Oper op (partialEval e1) (partialEval e2)
partialEval (Function f e) = Function f (partialEval e)


numberParser :: Parser Number
numberParser = try decimalNumber <|> integerNumber
  where
    decimalNumber :: Parser Number
    decimalNumber = DecimalNum <$> try (read <$> (Parsec.many digit <* char '.' <* Parsec.many digit))

    integerNumber :: Parser Number
    integerNumber = IntegerNum <$> read <$> Parsec.many digit

varParser :: Parser Expr
varParser = Var <$> many1 letter

exprParser :: Parser Expr
exprParser = spaces *> termParser <* spaces
  where
    termParser :: Parser Expr
    termParser = chainl1 factorParser addOpParser

    factorParser :: Parser Expr
    factorParser = chainl1 atomParser mulOpParser

    atomParser :: Parser Expr
    atomParser = choice [try varParser, try (Val <$> numberParser), parens exprParser]

    addOpParser :: Parser (Expr -> Expr -> Expr)
    addOpParser = Oper <$> addOp
      where
        addOp = choice [char '+' >> return Add, char '-' >> return Subtract]

    mulOpParser :: Parser (Expr -> Expr -> Expr)
    mulOpParser = Oper <$> mulOp
      where
        mulOp = choice [char '*' >> return Multiply, char '/' >> return Divide]

parens :: Parser a -> Parser a
parens = between (char '(' *> spaces) (char ')' *> spaces)

calculate :: Expr -> IO ()
calculate expr = do
  let result = eval expr
  case result of
    Just value -> print value
    Nothing -> print (partialEval expr)

main :: IO ()
main = do
  putStrLn "Enter an expression:"
  input <- getLine
  case parse exprParser "" input of
    Left err -> print err
    Right expr -> do
      putStrLn "Result:"
      calculate expr
