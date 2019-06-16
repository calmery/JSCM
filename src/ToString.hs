module ToString (toString) where

import           AST (Expression (..))
import           RIO

toString :: Expression -> String
toString (JSProgram expressions) = unwords $ map toString expressions
toString (JSIdentifier identifier) = identifier
toString (JSBooleanLiteral boolean) = if boolean then "true" else "false"
toString (JSNumberLiteral number) = show number
toString (JSStringLiteral string) = "\"" ++ string ++ "\""
toString (JSArrayExpression xs) = "[" ++ (unwords $ map (\x -> toString x ++ ",") xs) ++ "]"
toString (JSAssignmentExpression identifier body) = toString identifier ++ " = " ++ toString body
toString (JSBinaryExpression operator x y) = "(" ++ toString x ++ " " ++ operator ++ " " ++ toString y ++ ")"
toString (JSCallExpression arguments identifier) = toString identifier ++ "(" ++ toStringArray arguments ++ ")\n"
toString (JSMemberExpression x y) = toString y ++ "[" ++ toString x ++ "]"
toString (JSObjectMemberExpression x y) = toString y ++ "." ++ toString x
toString (JSPostfixUpdateExpression operator x) = "(" ++ toString x ++ operator ++ ")"
toString (JSPrefixUpdateExpression operator x) = "(" ++ operator ++ toString x ++ ")"
toString (JSUnaryExpression operator x) = "(" ++ operator ++ toString x ++ ")"
toString (JSBlockStatement expressions) = "{\n" ++ (unlines $ map toString expressions) ++ "}"
toString (JSForStatement (x, y, z) body) = "for (" ++ toString x ++ "; " ++ toString y ++ "; " ++ toString z ++ ") " ++ toString body
toString (JSIfStatement condition body elseBody) = "if (" ++ toString condition ++ ") " ++ toString body ++ (case elseBody of
  Just elseBody' -> " else " ++ toString elseBody'
  Nothing        -> "")
toString (JSReturnStatement body) = "return " ++ toString body
toString (JSFunctionDeclaration (JSIdentifier identifier) arguments body) =
  "\n\nfunction " ++ identifier ++ "(" ++ toStringArray arguments ++ ")" ++ toString body
toString (JSVariableDeclaration identifier) = "var " ++ toString identifier
toString JSEmpty = ""
toString (JSInternalCode string) = string
toString _ = ""

toStringArray :: [Expression] -> String
toStringArray [] = ""
toStringArray [expression] = toString expression
toStringArray (expression:expressions) = toString expression ++ ", " ++ toStringArray expressions
