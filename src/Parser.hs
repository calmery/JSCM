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

data Expression
  = JSProgram [Expression]
  | JSIdentifier String
  | JSBooleanLiteral Bool
  | JSNumberLiteral Integer
  | JSStringLiteral String
  | JSArrayExpression [Expression]
  | JSAssignmentExpression Expression Expression
  | JSBinaryExpression String Expression Expression
  | JSCallExpression [Expression] Expression
  | JSMemberExpression Expression Expression
  | JSObjectMemberExpression Expression Expression
  | JSPostfixUpdateExpression String Expression
  | JSPrefixUpdateExpression String Expression
  | JSUnaryExpression String Expression
  | JSBlockStatement [Expression]
  | JSBreakStatement
  | JSContinueStatement
  | JSForStatement (Expression, Expression, Expression) Expression
  | JSIfStatement Expression Expression (Maybe Expression)
  | JSLabeledStatement Expression Expression
  | JSReturnStatement Expression
  | JSSwitchStatement Expression Expression
  | JSTryStatement Expression Expression Expression (Maybe Expression)
  | JSWhileStatement Expression Expression
  | JSFunctionDeclaration Expression [Expression] Expression
  | JSVariableDeclaration Expression
  | JSSwitchCase Expression Expression
  | JSSwitchDefault Expression
  | JSComment String
  | JSEmpty
  | JSInternalCode String
  deriving (Eq, Show)

-- haskell - Parsec.Expr repeated Prefix/Postfix operator not supported - Stack Overflow
-- https://stackoverflow.com/questions/10475337/parsec-expr-repeated-prefix-postfix-operator-not-supported
postfix p = Postfix . chainl1 p $ return (flip (.))
prefix p = Prefix . chainl1 p $ return (.)

braces = Token.braces tokenParser
brackets = Token.brackets tokenParser
colon = Token.colon tokenParser
comma = Token.comma tokenParser
commaSep = Token.commaSep tokenParser
dot = Token.dot tokenParser
parens = Token.parens tokenParser
reserved = Token.reserved tokenParser
reservedOp = Token.reservedOp tokenParser
semi = Token.semi tokenParser
symbol = Token.symbol tokenParser

-- Parsers

programParser :: Parser Expression
programParser = do
  skipMany firstSingleLineComment
  expressions <- many lineParser
  return $ JSProgram expressions
  where
    firstSingleLineComment = do
      char '/'
      char '/'
      _ <- many $ noneOf ['\n']
      skipMany newline

expressionsParser :: Parser Expression
expressionsParser = do
  expressions <- many lineParser
  return $ JSBlockStatement expressions

lineParser :: Parser Expression
lineParser = do
  expression <- expressionParser
  optional semi
  return expression

expressionParser :: Parser Expression
expressionParser = buildExpressionParser table parsers
  where
    table =
      [ [ (postfix . choice)
          [ JSObjectMemberExpression <$> (dot *> parsers)
          , JSMemberExpression <$> brackets expressionParser
          , JSCallExpression <$> (parens . commaSep) expressionParser
          , JSPostfixUpdateExpression "++" <$ reservedOp "++"
          , JSPostfixUpdateExpression "--" <$ reservedOp "--"
          ]
        ]
      , [ (prefix . choice)
          [ JSUnaryExpression "!" <$ reservedOp "!"
          , JSUnaryExpression "+" <$ reservedOp "+"
          , JSUnaryExpression "-" <$ reservedOp "-"
          , JSPrefixUpdateExpression "++" <$ reservedOp "++"
          , JSPrefixUpdateExpression "--" <$ reservedOp "--"
          ]
        ]
      , [ Infix (reservedOp "**" >> (return $ JSBinaryExpression "**")) AssocLeft
        , Infix (reservedOp "*" >> (return $ JSBinaryExpression "*")) AssocLeft
        , Infix (reservedOp "/" >> (return $ JSBinaryExpression "/")) AssocLeft
        , Infix (reservedOp "%" >> (return $ JSBinaryExpression "%")) AssocLeft
        ]
      , [ Infix (reservedOp "+" >> (return $ JSBinaryExpression "+")) AssocLeft
        , Infix (reservedOp "-" >> (return $ JSBinaryExpression "-")) AssocLeft
        ]
      , [ Infix (reservedOp ">" >> (return $ JSBinaryExpression ">")) AssocLeft
        , Infix (reservedOp ">=" >> (return $ JSBinaryExpression ">=")) AssocLeft
        , Infix (reservedOp "<" >> (return $ JSBinaryExpression "<")) AssocLeft
        , Infix (reservedOp "<=" >> (return $ JSBinaryExpression "<=")) AssocLeft
        ]
      , [ Infix (reservedOp "<<" >> (return $ JSBinaryExpression "<<")) AssocLeft
        , Infix (reservedOp ">>" >> (return $ JSBinaryExpression ">>")) AssocLeft
        ]
      , [ Infix (reservedOp "==" >> (return $ JSBinaryExpression "==")) AssocLeft
        , Infix (reservedOp "!=" >> (return $ JSBinaryExpression "!=")) AssocLeft
        , Infix (reservedOp "===" >> (return $ JSBinaryExpression "===")) AssocLeft
        , Infix (reservedOp "!==" >> (return $ JSBinaryExpression "!==")) AssocLeft
        ]
      , [ Infix (reservedOp "&" >> (return $ JSBinaryExpression "&")) AssocLeft
        , Infix (reservedOp "|" >> (return $ JSBinaryExpression "|")) AssocLeft
        ]
      , [ Infix (reservedOp "&&" >> (return $ JSBinaryExpression "&&")) AssocLeft
        , Infix (reservedOp "||" >> (return $ JSBinaryExpression "||")) AssocLeft
        ]
      , [ Infix (reservedOp "=" >> return JSAssignmentExpression) AssocLeft
        ]
      ]

parsers :: Parser Expression
parsers = parensParser
  <|> blockStatementParser
  <|> breakStatementParser
  <|> continueStatementParser
  <|> forStatementParser
  <|> ifStatementParser
  <|> returnStatementParser
  <|> switchStatementParser
  <|> tryStatementParser
  <|> whileStatementParser
  <|> functionDeclarationParser
  <|> variableDeclarationParser
  <|> arrayExpressionParser
  <|> booleanLiteralParser
  <|> numberLiteralParser
  <|> stringLiteralParser
  <|> identifierParser

parensParser :: Parser Expression
parensParser = parens expressionParser

-- Statement

blockStatementParser :: Parser Expression
blockStatementParser = try (braces hashParser) <|> braces expressionsParser
  where
    hashParser = do
      labels <- commaSep $ do
        label <- identifierParser
        colon
        JSLabeledStatement label <$> expressionParser
      return $ JSBlockStatement labels

breakStatementParser :: Parser Expression
breakStatementParser = do
  reserved "break"
  return JSBreakStatement

continueStatementParser :: Parser Expression
continueStatementParser = do
  reserved "continue"
  return JSContinueStatement

forStatementParser :: Parser Expression
forStatementParser = do
  reserved "for"
  expressions <- parens $ do
    one <- e <|> semi $> JSEmpty
    two <- e <|> semi $> JSEmpty
    three <- expressionParser <|> return JSEmpty
    return (one, two, three)
  JSForStatement expressions <$> expressionParser
  where
    e = do
      expression <- expressionParser
      semi
      return expression

ifStatementParser :: Parser Expression
ifStatementParser = do
  reserved "if"
  expression <- parens expressionParser
  expressions <- expressionParser
  elseExpressions <- optionMaybe $ do
    reserved "else"
    expressionParser
  return $ JSIfStatement expression expressions elseExpressions

returnStatementParser :: Parser Expression
returnStatementParser = do
  reserved "return"
  JSReturnStatement <$> expressionParser

switchStatementParser :: Parser Expression
switchStatementParser = do
  reserved "switch"
  expression <- parens expressionParser
  block <- braces switchBlockParser
  return $ JSSwitchStatement expression block
  where
    switchBlockParser = do
      expressions <- many $ switchCaseParser <|> switchDefaultParser
      return $ JSBlockStatement expressions
    switchCaseParser = do
      reserved "case"
      expression <- expressionParser
      char ':'
      skipMany $ newline <|> space
      JSSwitchCase expression <$> expressionsParser
    switchDefaultParser = do
      reserved "default"
      char ':'
      skipMany $ newline <|> space
      JSSwitchDefault <$> expressionsParser

tryStatementParser :: Parser Expression
tryStatementParser = do
  reserved "try"
  tryExpressions <- expressionParser
  reserved "catch"
  _ <- parens $ return JSEmpty -- catch の引数
  catchExpressions <- expressionParser
  finallyExpressions <- optionMaybe $ do
    reserved "finally"
    expressionParser
  return $ JSTryStatement tryExpressions JSEmpty catchExpressions finallyExpressions

whileStatementParser :: Parser Expression
whileStatementParser = do
  reserved "while"
  expression <- parens expressionParser
  JSWhileStatement expression <$> expressionParser

-- Declaration

functionDeclarationParser :: Parser Expression
functionDeclarationParser = do
  reserved "function"
  name <- identifierParser
  arguments <- parens $ many $ do
    label <- identifierParser
    optional comma
    return label
  JSFunctionDeclaration name arguments <$> expressionParser

variableDeclarationParser :: Parser Expression
variableDeclarationParser = do
  reserved "var"
  JSVariableDeclaration <$> identifierParser

-- Expression

arrayExpressionParser :: Parser Expression
arrayExpressionParser = brackets $ do
  expressions <- many $ do
    expression <- expressionParser
    optional comma
    return expression
  return $ JSArrayExpression expressions

-- Literal

booleanLiteralParser :: Parser Expression
booleanLiteralParser = true <|> false
  where
    true = do
      reserved "true"
      return $ JSBooleanLiteral True
    false = do
      reserved "false"
      return $ JSBooleanLiteral False

numberLiteralParser :: Parser Expression
numberLiteralParser = do
  xs <- Token.integer tokenParser
  return $ JSNumberLiteral xs

stringLiteralParser :: Parser Expression
stringLiteralParser = do
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
  return $ JSStringLiteral $ concat xxs

-- Identifier

identifierParser :: Parser Expression
identifierParser = do
  xs <- Token.identifier tokenParser
  return $ JSIdentifier xs
