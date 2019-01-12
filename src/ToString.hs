module ToString (toString) where

import           Parser (Expression (..))
import           RIO

toString :: Expression -> String
toString (JSProgram expressions) = unwords $ map toString expressions
toString (JSBlock expressions) = "{\n" ++ (unwords $ map toString expressions) ++ "\n}"
toString (JSNumber number) = show number
toString (JSNativeCode x) = x
toString (JSPlus x y) = "(" ++ toString x ++ " + " ++ toString y ++ ")"
toString (JSMinus x y) = "(" ++ toString x ++ " - " ++ toString y ++ ")"
toString (JSTimes x y) = "(" ++ toString x ++ " * " ++ toString y ++ ")"
toString (JSDivide x y) = "(" ++ toString x ++ " / " ++ toString y ++ ")"
toString (JSModulo x y) = "(" ++ toString x ++ " % " ++ toString y ++ ")"
toString (JSExponentiation x y) = "(" ++ toString x ++ " ** " ++ toString y ++ ")"
toString (JSLooseEqual x y) = "(" ++ toString x ++ " == " ++ toString y ++ ")"
toString (JSLooseNotEqual x y) = "(" ++ toString x ++ " != " ++ toString y ++ ")"
toString (JSStrictEqual x y) = "(" ++ toString x ++ " === " ++ toString y ++ ")"
toString (JSStrictNotEqual x y) = "(" ++ toString x ++ " !== " ++ toString y ++ ")"
toString (JSGreater x y) = "(" ++ toString x ++ " > " ++ toString y ++ ")"
toString (JSGreaterOrEqual x y) = "(" ++ toString x ++ " >= " ++ toString y ++ ")"
toString (JSLess x y) = "(" ++ toString x ++ " < " ++ toString y ++ ")"
toString (JSLessOrEqual x y) = "(" ++ toString x ++ " <= " ++ toString y ++ ")"
toString (JSBoolean boolean) = if boolean then "true" else "false"
toString JSContinue = "continue\n"
toString JSBreak = "break\n"
toString (JSWhile x y) = "while (" ++ toString x ++ ")" ++ toString y ++ "\n"
toString (JSAssignment x y) = toString x ++ " = " ++ toString y ++ "\n"
toString (JSIdentifier identifier) = identifier
toString (JSVariableDeclaration x) = "var " ++ toString x
toString (JSIf x y z) = "if (" ++ toString x ++ ")" ++ toString y ++ (case z of
  Just z' -> " else " ++ toString z'
  Nothing -> "") ++ "\n"
toString (JSFor (x, y, z) a) = "for (" ++ toString x ++ "; " ++ toString y ++ "; " ++ toString z ++ ")" ++ toString a ++ "\n"
toString JSEmpty                = ""
toString (JSTryCatch tryExpressions JSEmpty catchExpressions finallyExpressions) =
  "try"
    ++ toString tryExpressions
    ++ "catch (_)"
    ++ toString catchExpressions
    ++ (case finallyExpressions of
      Just f  -> "finally" ++ toString f
      Nothing -> "")
    ++ "\n"
toString (JSSwitch x y) = "switch (" ++ toString x ++ ")" ++ toString y ++ "\n"
toString (JSCase x y) = "case " ++ toString x ++ ": " ++ toString y
toString (JSDefault x) = "default: " ++ toString x
toString (JSString x) = "\"" ++ x ++ "\""
toString (JSFunctionDeclaration (JSIdentifier x) ys z) = "function " ++ x ++ "(" ++ (unwords $ map (\y -> toString y ++ ",") ys) ++ ")" ++ toString z ++ "\n"
toString (JSReturn x) = "return " ++ toString x
toString (JSArray xs) = "[" ++ (unwords $ map (\x -> toString x ++ ",") xs) ++ "]"
toString (JSAndLogical x y) = toString x ++ " && " ++ toString y
toString (JSOrLogical x y) = toString x ++ " || " ++ toString y
toString (JSPrefixNot x) = "!" ++ toString x
toString (JSCall xs y) = toString y ++ "(" ++ (unwords $ map (\x -> toString x ++ ",") xs) ++ ")" ++ "\n"
toString (JSLabeled x y) = toString x ++ ": " ++ toString y ++ ","
toString (JSObjectMember x y) = toString y ++ "." ++ toString x
toString (JSMember x y) = toString y ++ "[" ++ toString x ++ "]"
toString (JSPrefixPlus x) = "(" ++ "+" ++ toString x ++ ")"
toString (JSPrefixMinus x) = "(" ++ "-" ++ toString x ++ ")"
toString (JSPrefixPlusUpdate x) = "(" ++ "++" ++ toString x ++ ")"
toString (JSPrefixMinusUpdate x) = "(" ++ "--" ++ toString x ++ ")"
toString (JSBigInt x) = show x ++ "n"
toString (JSAnd x y) = "(" ++ toString x ++ " & " ++ toString y ++ ")"
toString (JSOr x y) = "(" ++ toString x ++ " | " ++ toString y ++ ")"
toString (JSLeftShift x y) = "(" ++ toString x ++ " << " ++ toString y ++ ")"
toString (JSRightShift x y) = "(" ++ toString x ++ " >> " ++ toString y ++ ")"
toString (JSPostfixPlusUpdate x) = "(" ++ toString x ++ "++" ++ ")"
toString (JSPostfixMinusUpdate x) = "(" ++ toString x ++ "--" ++ ")"
toString _                      = ""
