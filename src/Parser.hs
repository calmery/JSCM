module Parser (parse, Expression(..)) where

import           RIO                    hiding (many, optional)
import           Text.Parsec            (many)
import qualified Text.Parsec            as Parsec
import           Text.Parsec.Char       (char)
import           Text.Parsec.Combinator (optionMaybe, optional)
import           Text.Parsec.Expr       (Assoc (AssocLeft), Operator (Infix),
                                         buildExpressionParser)
import           Text.Parsec.Language   (emptyDef)
import           Text.Parsec.String     (Parser)
import           Text.Parsec.Token      (LanguageDef, TokenParser,
                                         makeTokenParser, reservedNames,
                                         reservedOpNames)
import qualified Text.Parsec.Token      as Token

parse :: String -> Either Parsec.ParseError Expression
parse = Parsec.parse programParser "JavaScript"

keywords :: [String]
keywords =
  [ "true"
  , "false"
  , "while"
  , "continue"
  , "break"
  , "if"
  , "else"
  , "for"
  , "try"
  , "catch"
  , "switch"
  , "case"
  , "default"
  ]

operatorNames :: [String]
operatorNames =
  [ "+"
  , "-"
  , "*"
  , "/"
  , "%"
  , "**"
  , "=="
  , "!="
  , "==="
  , "!=="
  , ">"
  , ">="
  , "<"
  , "<="
  ]

languageDef :: LanguageDef st
languageDef = emptyDef
  { reservedNames = keywords
  , reservedOpNames = operatorNames
  }

tokenParser :: TokenParser ()
tokenParser = makeTokenParser languageDef

reservedKeywords :: String -> Parser ()
reservedKeywords = Token.reserved tokenParser

reservedOperatorNames :: String -> Parsec.ParsecT String () Identity ()
reservedOperatorNames = Token.reservedOp tokenParser

parens = Token.parens tokenParser
braces = Token.braces tokenParser
semi = Token.semi tokenParser

boolean :: Parser Expression
boolean = true <|> false
  where
    true = do
      reservedKeywords "true"
      return $ JSBoolean True
    false = do
      reservedKeywords "false"
      return $ JSBoolean False

integer :: Parser Integer
integer = Token.integer tokenParser

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
  deriving (Eq, Show)

programParser :: Parser Expression
programParser = do
  expressions <- many expressionParser
  return $ JSProgram expressions

expressionParser :: Parser Expression
expressionParser = buildExpressionParser table termParser

table :: [[Text.Parsec.Expr.Operator String () Identity Expression]]
table =
  [ [ Infix (reservedOperatorNames "**" >> return JSExponentiation) AssocLeft
    , Infix (reservedOperatorNames "*" >> return JSTimes) AssocLeft
    , Infix (reservedOperatorNames "/" >> return JSDivide) AssocLeft
    , Infix (reservedOperatorNames "%" >> return JSModulo) AssocLeft
    ]
  , [ Infix (reservedOperatorNames "+" >> return JSPlus) AssocLeft
    , Infix (reservedOperatorNames "-" >> return JSMinus) AssocLeft
    ]
  , [ Infix (reservedOperatorNames ">" >> return JSGreater) AssocLeft
    , Infix (reservedOperatorNames ">=" >> return JSGreaterOrEqual) AssocLeft
    , Infix (reservedOperatorNames "<" >> return JSLess) AssocLeft
    , Infix (reservedOperatorNames "<=" >> return JSLessOrEqual) AssocLeft
    ]
  , [ Infix (reservedOperatorNames "==" >> return JSLooseEqual) AssocLeft
    , Infix (reservedOperatorNames "!=" >> return JSLooseNotEqual) AssocLeft
    , Infix (reservedOperatorNames "===" >> return JSStrictEqual) AssocLeft
    , Infix (reservedOperatorNames "!==" >> return JSStrictNotEqual) AssocLeft
    ]
  ]

termParser :: Parser Expression
termParser = parens expressionParser
  <|> blockParser
  <|> whileParser
  <|> ifParser
  <|> forParser
  <|> tryCatchParser
  <|> switchParser
  <|> do
    v <- continueParser <|> breakParser <|> boolean <|> (JSNumber <$> integer)
    optional semi
    return v

blockParser :: Parser Expression
blockParser = braces expressionsParser

expressionsParser :: Parser Expression
expressionsParser = do
  expressions <- many expressionParser
  return $ JSBlock expressions

whileParser :: Parser Expression
whileParser = do
  reservedKeywords "while"
  expression <- parens expressionParser
  JSWhile expression <$> expressionParser

continueParser :: Parser Expression
continueParser = do
  reservedKeywords "continue"
  return JSContinue

breakParser :: Parser Expression
breakParser = do
  reservedKeywords "break"
  return JSBreak

ifParser :: Parser Expression
ifParser = do
  reservedKeywords "if"
  expression <- parens expressionParser
  block <- expressionParser
  elseBlock <- optionMaybe $ do
    reservedKeywords "else"
    expressionParser
  return $ JSIf expression block elseBlock

forParser :: Parser Expression
forParser = do
  reservedKeywords "for"
  expressions <- parens $ do
    one <- expressionParser <|> semi *> empty
    two <- expressionParser <|> semi *> empty
    three <- expressionParser <|> empty
    return (one, two, three)
  JSFor expressions <$> expressionParser
  where
    empty =
      return JSEmpty

tryCatchParser :: Parser Expression
tryCatchParser = do
  reservedKeywords "try"
  tryBlock <- braces $ do
    block <- many expressionParser
    return $ JSBlock block
  reservedKeywords "catch"
  arguments <- parens $ return JSEmpty
  catchBlock <- braces $ do
    block <- many expressionParser
    return $ JSBlock block
  finallyBlock <- optionMaybe $ do
    reservedKeywords "finally"
    block <- braces $ many expressionParser
    return $ JSBlock block
  return $ JSTryCatch tryBlock arguments catchBlock finallyBlock

switchParser :: Parser Expression
switchParser = do
  reservedKeywords "switch"
  expression <- parens expressionParser
  block <- braces switchBlockParser
  return $ JSSwitch expression block
  where
    switchBlockParser = do
      expressions <- many $ switchCaseParser <|> switchDefaultParser
      return $ JSBlock expressions
    switchCaseParser = do
      reservedKeywords "case"
      expression <- expressionParser
      char ':'
      JSCase expression <$> expressionsParser
    switchDefaultParser = do
      reservedKeywords "default"
      char ':'
      JSDefault <$> expressionsParser
