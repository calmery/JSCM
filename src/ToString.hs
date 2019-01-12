module ToString (toString) where

import           Parser (Expression (..))
import           RIO

toString :: Expression -> String
toString (JSProgram expressions) = unwords $ map toString expressions
toString (JSIdentifier identifier) = identifier
toString (JSBooleanLiteral boolean) = if boolean then "true" else "false"
toString (JSNumberLiteral number) = show number
toString (JSStringLiteral string) = "\"" ++ string ++ "\""
toString (JSArrayExpression xs) = "[" ++ (unwords $ map (\x -> toString x ++ ",") xs) ++ "]"
toString (JSAssignmentExpression identifier body) = toString identifier ++ " = " ++ toString body ++ "\n"
toString (JSBinaryExpression operator x y) = "(" ++ toString x ++ operator ++ toString y ++ ")"
toString (JSCallExpression arguments identifier) = toString identifier ++ "(" ++ (unwords $ map (\argument -> toString argument ++ ",") arguments) ++ ")" ++ "\n"
toString (JSMemberExpression x y) = toString y ++ "[" ++ toString x ++ "]"
toString (JSObjectMemberExpression x y) = toString y ++ "." ++ toString x
toString (JSPostfixUpdateExpression operator x) = "(" ++ toString x ++ operator ++ ")"
toString (JSPrefixUpdateExpression operator x) = "(" ++ operator ++ toString x ++ ")"
toString (JSUnaryExpression operator x) = "(" ++ operator ++ toString x ++ ")"
toString (JSBlockStatement expressions) = "{\n" ++ (unwords $ map toString expressions) ++ "\n}"
toString JSBreakStatement = "break\n"
toString JSContinueStatement = "continue\n"
toString (JSForStatement (x, y, z) body) = "for (" ++ toString x ++ "; " ++ toString y ++ "; " ++ toString z ++ ")" ++ toString body ++ "\n"
toString (JSIfStatement condition body elseBody) = "if (" ++ toString condition ++ ")" ++ toString body ++ (case elseBody of
  Just elseBody' -> " else " ++ toString elseBody'
  Nothing        -> "") ++ "\n"
toString (JSLabeledStatement identifier body) = toString identifier ++ ": " ++ toString body ++ ","
toString (JSReturnStatement body) = "return " ++ toString body ++ ";\n"
toString (JSSwitchStatement condition body) = "switch (" ++ toString condition ++ ")" ++ toString body ++ "\n"
toString (JSTryStatement body JSEmpty catchBody finallyBody) =
  "try"
    ++ toString body
    ++ "catch (_)"
    ++ toString catchBody
    ++ (case finallyBody of
      Just finallyBody' -> "finally" ++ toString finallyBody'
      Nothing           -> "")
    ++ "\n"
toString (JSWhileStatement condition body) = "while (" ++ toString condition ++ ")" ++ toString body ++ "\n"
toString (JSFunctionDeclaration (JSIdentifier identifier) arguments body) =
  "function " ++ identifier ++ "(" ++ (unwords $ map (\argument -> toString argument ++ ",") arguments) ++ ")" ++ toString body ++ "\n"
toString (JSVariableDeclaration identifier) = "var " ++ toString identifier
toString (JSSwitchCase identifier body) = "case " ++ toString identifier ++ ": " ++ toString body
toString (JSSwitchDefault body) = "default: " ++ toString body
toString JSEmpty = ""
toString (JSInternalCode string) = string
toString _ = ""
