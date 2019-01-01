module Parser (parse, Expression(..)) where

import           RIO                    hiding (many, optional, try)
import           Text.Parsec            (many, skipMany)
import qualified Text.Parsec            as Parsec
import           Text.Parsec.Char       (char, newline, noneOf, oneOf, space,
                                         tab)
import           Text.Parsec.Combinator (chainl1, choice, optionMaybe, optional)
import           Text.Parsec.Expr       (Assoc (AssocLeft),
                                         Operator (Infix, Postfix, Prefix),
                                         buildExpressionParser)
import           Text.Parsec.Prim       (try)
import           Text.Parsec.String     (Parser)
import qualified Text.Parsec.Token      as Token
import           TokenParser            (tokenParser)

parse :: String -> String -> Either Parsec.ParseError Expression
parse = Parsec.parse programParser

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
  | JSAssignment Expression Expression
  | JSIdentifier String
  | JSVariableDeclaration Expression
  | JSIf Expression Expression (Maybe Expression)
  | JSFor (Expression, Expression, Expression) Expression
  | JSEmpty
  | JSTryCatch Expression Expression Expression (Maybe Expression)
  | JSSwitch Expression Expression
  | JSCase Expression Expression
  | JSDefault Expression
  | JSString String
  | JSFunctionDeclaration Expression [Expression] Expression
  | JSReturn Expression
  | JSArray [Expression]
  | JSAndLogical Expression Expression
  | JSOrLogical Expression Expression
  | JSPrefixNot Expression
  | JSCall [Expression] Expression
  | JSLabeled Expression Expression
  | JSMember Expression Expression
  | JSPrefixPlus Expression
  | JSPrefixMinus Expression
  | JSPrefixPlusUpdate Expression
  | JSPrefixMinusUpdate Expression
  | JSBigInt Integer
  | JSAnd Expression Expression
  deriving (Eq, Show)

-- Token Parsers

-- https://stackoverflow.com/questions/10475337/parsec-expr-repeated-prefix-postfix-operator-not-supported?lq=1
postfix p = Postfix . chainl1 p $ return (flip (.))
prefix p = Prefix . chainl1 p $ return (.)

reserved :: String -> Parser ()
reserved = Token.reserved tokenParser

reservedOp :: String -> Parsec.ParsecT String () Identity ()
reservedOp = Token.reservedOp tokenParser

symbol :: String -> Parsec.ParsecT String () Identity String
symbol = Token.symbol tokenParser

commaSep = Token.commaSep tokenParser
parens = Token.parens tokenParser
braces = Token.braces tokenParser
brackets = Token.brackets tokenParser
semi = Token.semi tokenParser
comma = Token.comma tokenParser
colon = Token.colon tokenParser
dot = Token.dot tokenParser

-- Parsers

programParser :: Parser Expression
programParser = do
  expressions <- many $ do
    expression <- expressionParser
    optional semi
    return expression
  return $ JSProgram expressions

expressionsParser :: Parser Expression
expressionsParser = do
  expressions <- many $ do
    expression <- expressionParser
    optional semi
    return expression
  return $ JSBlock expressions

expressionParser :: Parser Expression
expressionParser = buildExpressionParser table parsers
  where
    table =
      [ [ (postfix . choice)
          [ JSMember <$> (dot *> expressionParser)
          , JSMember <$> brackets expressionParser
          , JSCall <$> (parens . commaSep) expressionParser
          ]
        ]
      , [ (prefix . choice)
          [ (JSPrefixNot <$ reservedOp "!")
          , (JSPrefixPlus <$ reservedOp "+")
          , (JSPrefixMinus <$ reservedOp "-")
          , (JSPrefixPlusUpdate <$ reservedOp "++")
          , (JSPrefixMinusUpdate <$ reservedOp "--")
          ]
        ]
      , [ Infix (reservedOp "**" >> return JSExponentiation) AssocLeft
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
      , [ Infix (reservedOp "&" >> return JSAnd) AssocLeft
        ]
      , [ Infix (reservedOp "&&" >> return JSAndLogical) AssocLeft
        , Infix (reservedOp "||" >> return JSOrLogical) AssocLeft
        ]
      , [ Infix (reservedOp "=" >> return JSAssignment) AssocLeft
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
  <|> variableParser
  <|> functionParser
  <|> returnParser
  <|> arrayParser
  <|> continueParser
  <|> breakParser
  <|> boolean
  <|> try bigint
  <|> integer
  <|> string
  <|> identifier

-- Statements

parensParser :: Parser Expression
parensParser = parens expressionParser -- 括弧付き

blockParser :: Parser Expression
blockParser = try (braces hashParser) <|> braces expressionsParser
  where
    hashParser = do
      labels <- commaSep $ do
        label <- identifier
        colon
        expression <- expressionParser
        return $ JSLabeled label expression
      return $ JSBlock labels

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
    one <- e <|> semi $> JSEmpty
    two <- e <|> semi $> JSEmpty
    three <- expressionParser <|> return JSEmpty
    return (one, two, three)
  JSFor expressions <$> expressionParser
  where
    e = do
      expression <- expressionParser
      semi
      return expression

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

variableParser :: Parser Expression
variableParser = do
  reserved "var"
  label <- identifier
  return $ JSVariableDeclaration label

functionParser :: Parser Expression
functionParser = do
  reserved "function"
  name <- identifier
  arguments <- parens $ many $ do
    label <- identifier
    optional comma
    return label
  expressions <- expressionParser
  return $ JSFunctionDeclaration name arguments expressions

returnParser :: Parser Expression
returnParser = do
  reserved "return"
  expression <- expressionParser
  return $ JSReturn expression

arrayParser :: Parser Expression
arrayParser = brackets $ do
  expressions <- many $ do
    expression <- expressionParser
    optional comma
    return expression
  return $ JSArray expressions

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

bigint :: Parser Expression
bigint = do
  xs <- Token.integer tokenParser
  symbol "n"
  return $ JSBigInt xs

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

identifier :: Parser Expression
identifier = do
  xs <- Token.identifier tokenParser
  return $ JSIdentifier xs
