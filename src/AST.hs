module AST (Expression(..)) where

import           RIO hiding (many, optional, try)

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
