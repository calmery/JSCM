module Parser (parse, Expression(..)) where

import           RIO                    hiding (many, optional)
import           Text.Parsec            (many, skipMany)
import qualified Text.Parsec            as Parsec
import           Text.Parsec.Char       (char, newline, noneOf, oneOf, space,
                                         tab)
import           Text.Parsec.Combinator (optionMaybe, optional)
import           Text.Parsec.Expr       (Assoc (AssocLeft), Operator (Infix),
                                         buildExpressionParser)
import           Text.Parsec.String     (Parser)
import qualified Text.Parsec.Token      as Token
import           TokenParser            (tokenParser)

parse :: String -> Either Parsec.ParseError Expression
parse = Parsec.parse programParser "JavaScript"

-- Data

data Expression
  = JSProgram [Expression]
  | JSBlock [Expression]
  | JSNumber Integer
  | JSPlus Expression Expression
  | JSMinus Expression Expression
  | JSTimes Expression Expression
  | JSDivide Expression Expression
  | JSModulo Expression Expression
  | JSExponentiation Expression Expression
  | JSLooseEqual Expression Expression
  | JSLooseNotEqual Expression Expression
  | JSStrictEqual Expression Expression
  | JSStrictNotEqual Expression Expression
  | JSGreater Expression Expression
  | JSGreaterOrEqual Expression Expression
  | JSLess Expression Expression
  | JSLessOrEqual Expression Expression
  | JSBoolean Bool
  | JSWhile Expression Expression
  | JSContinue
  | JSBreak
  | JSIf Expression Expression (Maybe Expression)
  | JSFor (Expression, Expression, Expression) Expression
  | JSEmpty
  | JSTryCatch Expression Expression Expression (Maybe Expression)
  | JSSwitch Expression Expression
  | JSCase Expression Expression
  | JSDefault Expression
  | JSString String
  deriving (Eq, Show)

-- Token Parsers

reserved :: String -> Parser ()
reserved = Token.reserved tokenParser

reservedOp :: String -> Parsec.ParsecT String () Identity ()
reservedOp = Token.reservedOp tokenParser

parens = Token.parens tokenParser
braces = Token.braces tokenParser
semi = Token.semi tokenParser

-- Parsers

programParser :: Parser Expression
programParser = do
  expressions <- many expressionParser
  return $ JSProgram expressions

expressionsParser :: Parser Expression
expressionsParser = do
  expressions <- many expressionParser
  return $ JSBlock expressions

expressionParser :: Parser Expression
expressionParser = buildExpressionParser table parsers
  where
    table =
      [ [ Infix (reservedOp "**" >> return JSExponentiation) AssocLeft
        , Infix (reservedOp "*" >> return JSTimes) AssocLeft
        , Infix (reservedOp "/" >> return JSDivide) AssocLeft
        , Infix (reservedOp "%" >> return JSModulo) AssocLeft
        ]
      , [ Infix (reservedOp "+" >> return JSPlus) AssocLeft
        , Infix (reservedOp "-" >> return JSMinus) AssocLeft
        ]
      , [ Infix (reservedOp ">" >> return JSGreater) AssocLeft
        , Infix (reservedOp ">=" >> return JSGreaterOrEqual) AssocLeft
        , Infix (reservedOp "<" >> return JSLess) AssocLeft
        , Infix (reservedOp "<=" >> return JSLessOrEqual) AssocLeft
        ]
      , [ Infix (reservedOp "==" >> return JSLooseEqual) AssocLeft
        , Infix (reservedOp "!=" >> return JSLooseNotEqual) AssocLeft
        , Infix (reservedOp "===" >> return JSStrictEqual) AssocLeft
        , Infix (reservedOp "!==" >> return JSStrictNotEqual) AssocLeft
        ]
      ]

parsers :: Parser Expression
parsers = parensParser
  <|> blockParser
  <|> whileParser
  <|> ifParser
  <|> forParser
  <|> tryCatchParser
  <|> switchParser
  <|> do
    value <- continueParser
      <|> breakParser
      <|> boolean
      <|> integer
      <|> string
    optional semi
    return value

-- Statements

parensParser :: Parser Expression
parensParser = parens expressionParser -- 括弧付き

blockParser :: Parser Expression
blockParser = braces expressionsParser -- 複数行のソースコードをパースする

whileParser :: Parser Expression
whileParser = do
  reserved "while"
  expression <- parens expressionParser
  JSWhile expression <$> expressionParser

ifParser :: Parser Expression
ifParser = do
  reserved "if"
  expression <- parens expressionParser
  expressions <- expressionParser
  elseExpressions <- optionMaybe $ do
    reserved "else"
    expressionParser
  return $ JSIf expression expressions elseExpressions

forParser :: Parser Expression
forParser = do
  reserved "for"
  expressions <- parens $ do
    one <- expressionParser <|> semi $> JSEmpty
    two <- expressionParser <|> semi $> JSEmpty
    three <- expressionParser <|> return JSEmpty
    return (one, two, three)
  JSFor expressions <$> expressionParser

tryCatchParser :: Parser Expression
tryCatchParser = do
  reserved "try"
  tryExpressions <- expressionParser
  reserved "catch"
  _ <- parens $ return JSEmpty -- catch の引数
  catchExpressions <- expressionParser
  finallyExpressions <- optionMaybe $ do
    reserved "finally"
    expressionParser
  return $ JSTryCatch tryExpressions JSEmpty catchExpressions finallyExpressions

switchParser :: Parser Expression
switchParser = do
  reserved "switch"
  expression <- parens expressionParser
  block <- braces switchBlockParser
  return $ JSSwitch expression block
  where
    switchBlockParser = do
      expressions <- many $ switchCaseParser <|> switchDefaultParser
      return $ JSBlock expressions
    switchCaseParser = do
      reserved "case"
      expression <- expressionParser
      char ':'
      JSCase expression <$> expressionsParser
    switchDefaultParser = do
      reserved "default"
      char ':'
      JSDefault <$> expressionsParser

-- Values

continueParser :: Parser Expression
continueParser = do
  reserved "continue"
  return JSContinue

breakParser :: Parser Expression
breakParser = do
  reserved "break"
  return JSBreak

boolean :: Parser Expression
boolean = true <|> false
  where
    true = do
      reserved "true"
      return $ JSBoolean True
    false = do
      reserved "false"
      return $ JSBoolean False

integer :: Parser Expression
integer = do
  xs <- Token.integer tokenParser
  return $ JSNumber xs

string :: Parser Expression
string = do
  singleOrDouble <- char '"' <|> char '\''
  let targets = singleOrDouble:['\\', '\n', '\r', '\v', '\t', '\b', '\f']
  xxs <- many $ do
    x <- noneOf targets
    return [x]
    <|> do
      backslashes <- char '\\'
      target <- oneOf targets
      return [backslashes, target]
  char singleOrDouble
  skipMany $ space <|> tab <|> newline
  return $ JSString $ concat xxs
