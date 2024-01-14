{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}

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
eval (Val n) = evalNumber n
eval (Oper op e1 e2) = applyOperation op (eval e1) (eval e2)

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

