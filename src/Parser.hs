module Parser (parse) where

import           RIO
import qualified Text.Parsec          as Parsec
import           Text.Parsec.Expr     (Assoc (AssocLeft), Operator (Infix),
                                       buildExpressionParser)
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String   (Parser)
import           Text.Parsec.Token    (LanguageDef, TokenParser,
                                       makeTokenParser, reservedOpNames)
import qualified Text.Parsec.Token    as Token

parse :: String -> String
parse input =
  case Parsec.parse expressionParser "JavaScript" input of
    Left error ->
      show error

    Right formula ->
      show formula

operatorNames :: [String]
operatorNames =
  [ "+"
  , "-"
  , "*"
  , "/"
  ]

languageDef :: LanguageDef st
languageDef = emptyDef
  { reservedOpNames = operatorNames
  }

tokenParser :: TokenParser ()
tokenParser = makeTokenParser languageDef

integer :: Parser Integer
integer = Token.integer tokenParser

reservedOperatorNames :: String -> Parsec.ParsecT String () Identity ()
reservedOperatorNames = Token.reservedOp tokenParser

data Expression
  = JSNumber Integer
  | JSPlus Expression Expression
  | JSMinus Expression Expression
  | JSTimes Expression Expression
  | JSDivide Expression Expression
  deriving Show

expressionParser :: Parser Expression
expressionParser = buildExpressionParser table termParser

table :: [[Text.Parsec.Expr.Operator String () Identity Expression]]
table =
  [ [ Infix (reservedOperatorNames "*" >> return JSTimes) AssocLeft
    , Infix (reservedOperatorNames "/" >> return JSDivide) AssocLeft
    ]
  , [ Infix (reservedOperatorNames "+" >> return JSPlus) AssocLeft
    , Infix (reservedOperatorNames "-" >> return JSMinus) AssocLeft
    ]
  ]

termParser :: Parser Expression
termParser = JSNumber <$> integer
