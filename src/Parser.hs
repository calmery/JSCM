module Parser (parse, Expression(..)) where

import           RIO                    hiding (many)
import           Text.Parsec            (many)
import qualified Text.Parsec            as Parsec
import           Text.Parsec.Combinator (optionMaybe)
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
  <|> continueParser
  <|> breakParser
  <|> ifParser
  <|> boolean
  <|> (JSNumber <$> integer)

blockParser :: Parser Expression
blockParser = do
  expressions <- braces $ many expressionParser
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
