module ParserSpec (spec) where

import           Parser      (Expression (..))
import qualified Parser      as P
import           RIO
import           Test.Hspec
import qualified Text.Parsec as Parsec

parse :: String -> Either Parsec.ParseError Expression
parse = P.parse "Test"

commentSpec :: Spec
commentSpec =
  it "Comment" $ do
    parse "1 + 1 // Hello World 2 + 2" `shouldBe`
      Right (JSProgram [JSBinaryExpression "+" (JSNumberLiteral 1) (JSNumberLiteral 1)])
    parse "1 + 1 /* Hello World */ 2 + 2" `shouldBe`
      Right (JSProgram [JSBinaryExpression "+" (JSNumberLiteral 1) (JSNumberLiteral 1), JSBinaryExpression "+" (JSNumberLiteral 2) (JSNumberLiteral 2)])
    parse "1 + 1 /* /* Hello World */ */ 2 + 2" `shouldBe`
      Right (JSProgram [JSBinaryExpression "+" (JSNumberLiteral 1) (JSNumberLiteral 1), JSBinaryExpression "+" (JSNumberLiteral 2) (JSNumberLiteral 2)])

stringLiteralSpec :: Spec
stringLiteralSpec =
  it "String Literal" $ do
    parse "\"a\"" `shouldBe`
      Right (JSProgram [JSStringLiteral "a"])
    parse "\"abc\"" `shouldBe`
      Right (JSProgram [JSStringLiteral "abc"])
    parse "\"\\\"abc\\\"\"" `shouldBe`
      Right (JSProgram [JSStringLiteral "\\\"abc\\\""])
    parse "'a'" `shouldBe`
      Right (JSProgram [JSStringLiteral "a"])
    parse "'abc'" `shouldBe`
      Right (JSProgram [JSStringLiteral "abc"])
    parse "'\\'abc\\''" `shouldBe`
      Right (JSProgram [JSStringLiteral "\\'abc\\'"])

arithmeticOperatorsSpec :: Spec
arithmeticOperatorsSpec =
  describe "Arithmetic operators" $ do
    it "Addition" $ do
      parse "1+1" `shouldBe`
        Right (JSProgram [JSBinaryExpression "+" (JSNumberLiteral 1) (JSNumberLiteral 1)])
      parse "++1" `shouldBe`
        Right (JSProgram [JSPrefixUpdateExpression "++" (JSNumberLiteral 1)])
      parse "++1;\n++1" `shouldBe`
        Right (JSProgram [JSPrefixUpdateExpression "++" (JSNumberLiteral 1), JSPrefixUpdateExpression "++" (JSNumberLiteral 1)])
      parse "1++" `shouldBe`
        Right (JSProgram [JSPostfixUpdateExpression "++" (JSNumberLiteral 1)])
      parse "1++;\n1++" `shouldBe`
        Right (JSProgram [JSPostfixUpdateExpression "++" (JSNumberLiteral 1), JSPostfixUpdateExpression "++" (JSNumberLiteral 1)])
    it "Subtraction" $ do
      parse "1-1" `shouldBe`
        Right (JSProgram [JSBinaryExpression "-" (JSNumberLiteral 1) (JSNumberLiteral 1)])
      parse "--1" `shouldBe`
        Right (JSProgram [JSPrefixUpdateExpression "--" (JSNumberLiteral 1)])
      parse "--1;\n--1" `shouldBe`
        Right (JSProgram [JSPrefixUpdateExpression "--" (JSNumberLiteral 1), JSPrefixUpdateExpression "--" (JSNumberLiteral 1)])
      parse "1--" `shouldBe`
        Right (JSProgram [JSPostfixUpdateExpression "--" (JSNumberLiteral 1)])
      parse "1--;\n1--" `shouldBe`
        Right (JSProgram [JSPostfixUpdateExpression "--" (JSNumberLiteral 1), JSPostfixUpdateExpression "--" (JSNumberLiteral 1)])
    it "Multiplication" $ do
      parse "1*1" `shouldBe`
        Right (JSProgram [JSBinaryExpression "*" (JSNumberLiteral 1) (JSNumberLiteral 1)])
    it "Division" $ do
      parse "1/1" `shouldBe`
        Right (JSProgram [JSBinaryExpression "/" (JSNumberLiteral 1) (JSNumberLiteral 1)])
    it "Remainder" $ do
      parse "1%1" `shouldBe`
        Right (JSProgram [JSBinaryExpression "%" (JSNumberLiteral 1) (JSNumberLiteral 1)])
    it "Exponentiation" $ do
      parse "1**1" `shouldBe`
        Right (JSProgram [JSBinaryExpression "**" (JSNumberLiteral 1) (JSNumberLiteral 1)])
    it "Parentheses" $ do
      parse "(1+2)*3" `shouldBe`
        Right (JSProgram [JSBinaryExpression "*" (JSBinaryExpression "+" (JSNumberLiteral 1) (JSNumberLiteral 2)) (JSNumberLiteral 3)])
      parse "(1+2)/3" `shouldBe`
        Right (JSProgram [JSBinaryExpression "/" (JSBinaryExpression "+" (JSNumberLiteral 1) (JSNumberLiteral 2)) (JSNumberLiteral 3)])

comparisonOperatorsSpec :: Spec
comparisonOperatorsSpec =
  describe "Comparison operators" $ do
    it "Equality" $ do
      parse "1==1" `shouldBe`
        Right (JSProgram [JSBinaryExpression "==" (JSNumberLiteral 1) (JSNumberLiteral 1)])
    it "Inequality" $ do
      parse "1!=1" `shouldBe`
        Right (JSProgram [JSBinaryExpression "!=" (JSNumberLiteral 1) (JSNumberLiteral 1)])
    it "Strict Equality" $ do
      parse "1===1" `shouldBe`
        Right (JSProgram [JSBinaryExpression "===" (JSNumberLiteral 1) (JSNumberLiteral 1)])
    it "Strict Inequality" $ do
      parse "1!==1" `shouldBe`
        Right (JSProgram [JSBinaryExpression "!==" (JSNumberLiteral 1) (JSNumberLiteral 1)])
    it "Greater than operator" $ do
      parse "1>1" `shouldBe`
        Right (JSProgram [JSBinaryExpression ">" (JSNumberLiteral 1) (JSNumberLiteral 1)])
    it "Greater than or equal operator" $ do
      parse "1>=1" `shouldBe`
        Right (JSProgram [JSBinaryExpression ">=" (JSNumberLiteral 1) (JSNumberLiteral 1)])
    it "Less than operator" $ do
      parse "1<1" `shouldBe`
        Right (JSProgram [JSBinaryExpression "<" (JSNumberLiteral 1) (JSNumberLiteral 1)])
    it "Less than or equal operator" $ do
      parse "1<=1" `shouldBe`
        Right (JSProgram [JSBinaryExpression "<=" (JSNumberLiteral 1) (JSNumberLiteral 1)])

whileStatementSpec :: Spec
whileStatementSpec =
  it "While Statement" $ do
    parse "while (true) { 1 + 1 }" `shouldBe`
      Right (JSProgram [JSWhileStatement (JSBooleanLiteral True) (JSBlockStatement [JSBinaryExpression "+" (JSNumberLiteral 1) (JSNumberLiteral 1)])])
    parse "while (true) { continue }" `shouldBe`
      Right (JSProgram [JSWhileStatement (JSBooleanLiteral True) (JSBlockStatement [JSContinueStatement])])
    parse "while (true) { break }" `shouldBe`
      Right (JSProgram [JSWhileStatement (JSBooleanLiteral True) (JSBlockStatement [JSBreakStatement])])
    parse "while (true) 1 + 1" `shouldBe`
      Right (JSProgram [JSWhileStatement (JSBooleanLiteral True) (JSBinaryExpression "+" (JSNumberLiteral 1) (JSNumberLiteral 1))])

ifStatementSpec :: Spec
ifStatementSpec =
  it "If Statement" $ do
    parse "if (true) { 1 + 1 } else { 2 + 2 }" `shouldBe`
      Right (JSProgram [JSIfStatement (JSBooleanLiteral True) (JSBlockStatement [JSBinaryExpression "+" (JSNumberLiteral 1) (JSNumberLiteral 1)]) (Just (JSBlockStatement [JSBinaryExpression "+" (JSNumberLiteral 2) (JSNumberLiteral 2)]))])
    parse "if (true) { 1 + 1 } else if (true) { 2 + 2 }" `shouldBe`
      Right (JSProgram [JSIfStatement (JSBooleanLiteral True) (JSBlockStatement [JSBinaryExpression "+" (JSNumberLiteral 1) (JSNumberLiteral 1)]) (Just (JSIfStatement (JSBooleanLiteral True) (JSBlockStatement [JSBinaryExpression "+" (JSNumberLiteral 2) (JSNumberLiteral 2)]) Nothing))])
    parse "if (true) { 1 + 1 } else if (true) { 2 + 2 } else { 3 + 3 }" `shouldBe`
      Right (JSProgram [JSIfStatement (JSBooleanLiteral True) (JSBlockStatement [JSBinaryExpression "+" (JSNumberLiteral 1) (JSNumberLiteral 1)]) (Just (JSIfStatement (JSBooleanLiteral True) (JSBlockStatement [JSBinaryExpression "+" (JSNumberLiteral 2) (JSNumberLiteral 2)]) (Just (JSBlockStatement [JSBinaryExpression "+" (JSNumberLiteral 3) (JSNumberLiteral 3)]))))])

forStatementSpec :: Spec
forStatementSpec =
  it "For Statement" $ do
    parse "for (true; true; true) { 1 + 1 }" `shouldBe`
      Right (JSProgram [JSForStatement (JSBooleanLiteral True, JSBooleanLiteral True, JSBooleanLiteral True) (JSBlockStatement [JSBinaryExpression "+" (JSNumberLiteral 1) (JSNumberLiteral 1)])])
    parse "for (;;) { 1 + 1 }" `shouldBe`
      Right (JSProgram [JSForStatement (JSEmpty, JSEmpty, JSEmpty) (JSBlockStatement [JSBinaryExpression "+" (JSNumberLiteral 1) (JSNumberLiteral 1)])])

tryCatchStatementSpec :: Spec
tryCatchStatementSpec =
  it "Try Catch Statement" $ do
    parse "try { 1 + 1 } catch () { 2 + 2 }" `shouldBe`
      Right (JSProgram [JSTryStatement (JSBlockStatement [JSBinaryExpression "+" (JSNumberLiteral 1) (JSNumberLiteral 1)]) JSEmpty (JSBlockStatement [JSBinaryExpression "+" (JSNumberLiteral 2) (JSNumberLiteral 2)]) Nothing])
    parse "try { 1 + 1 } catch () { 2 + 2 } finally { 3 + 3 }" `shouldBe`
      Right (JSProgram [JSTryStatement (JSBlockStatement [JSBinaryExpression "+" (JSNumberLiteral 1) (JSNumberLiteral 1)]) JSEmpty (JSBlockStatement [JSBinaryExpression "+" (JSNumberLiteral 2) (JSNumberLiteral 2)]) (Just (JSBlockStatement [JSBinaryExpression "+" (JSNumberLiteral 3) (JSNumberLiteral 3)]))])

switchStatementSpec :: Spec
switchStatementSpec =
  it "Switch Statement" $ do
    parse "switch(true) { case true: 1 + 1; case false: 2 + 2; }" `shouldBe`
      Right (JSProgram [JSSwitchStatement (JSBooleanLiteral True) (JSBlockStatement [JSSwitchCase (JSBooleanLiteral True) (JSBlockStatement [JSBinaryExpression "+" (JSNumberLiteral 1) (JSNumberLiteral 1)]),JSSwitchCase (JSBooleanLiteral False) (JSBlockStatement [JSBinaryExpression "+" (JSNumberLiteral 2) (JSNumberLiteral 2)])])])
    parse "switch(true) { case true: 1 + 1; case false: 2 + 2; default: 3 + 3; }" `shouldBe`
      Right (JSProgram [JSSwitchStatement (JSBooleanLiteral True) (JSBlockStatement [JSSwitchCase (JSBooleanLiteral True) (JSBlockStatement [JSBinaryExpression "+" (JSNumberLiteral 1) (JSNumberLiteral 1)]),JSSwitchCase (JSBooleanLiteral False) (JSBlockStatement [JSBinaryExpression "+" (JSNumberLiteral 2) (JSNumberLiteral 2)]),JSSwitchDefault (JSBlockStatement [JSBinaryExpression "+" (JSNumberLiteral 3) (JSNumberLiteral 3)])])])

variableSpec :: Spec
variableSpec =
  it "Variable" $ do
    parse "x = 1" `shouldBe`
      Right (JSProgram [JSAssignmentExpression (JSIdentifier "x") (JSNumberLiteral 1)])
    parse "var x = 1" `shouldBe`
      Right (JSProgram [JSAssignmentExpression (JSVariableDeclaration (JSIdentifier "x")) (JSNumberLiteral 1)])

functionStatementSpec :: Spec
functionStatementSpec =
  it "Function Statement" $ do
    parse "function f() {}" `shouldBe`
      Right (JSProgram [JSFunctionDeclaration (JSIdentifier "f") [] (JSBlockStatement [])])
    parse "function f(x) {}" `shouldBe`
      Right (JSProgram [JSFunctionDeclaration (JSIdentifier "f") [JSIdentifier "x"] (JSBlockStatement [])])
    parse "function f(x, y) {}" `shouldBe`
      Right (JSProgram [JSFunctionDeclaration (JSIdentifier "f") [JSIdentifier "x", JSIdentifier "y"] (JSBlockStatement [])])
    parse "function f(x, y) { return x + y }" `shouldBe`
      Right (JSProgram [JSFunctionDeclaration (JSIdentifier "f") [JSIdentifier "x",JSIdentifier "y"] (JSBlockStatement [JSReturnStatement (JSBinaryExpression "+" (JSIdentifier "x") (JSIdentifier "y"))])])
    parse "f()" `shouldBe`
      Right (JSProgram [JSCallExpression [] (JSIdentifier "f")])
    parse "f(1, 2)" `shouldBe`
      Right (JSProgram [JSCallExpression [JSNumberLiteral 1,JSNumberLiteral 2] (JSIdentifier "f")])
    parse "f(1, 2)(3, 4)" `shouldBe`
      Right (JSProgram [JSCallExpression [JSNumberLiteral 3,JSNumberLiteral 4] (JSCallExpression [JSNumberLiteral 1,JSNumberLiteral 2] (JSIdentifier "f"))])

arrayStatementSpec :: Spec
arrayStatementSpec =
  it "Array Statement" $ do
    parse "[]" `shouldBe`
      Right (JSProgram [JSArrayExpression []])
    parse "[1]" `shouldBe`
      Right (JSProgram [JSArrayExpression [JSNumberLiteral 1]])
    parse "[1, 2, 3]" `shouldBe`
      Right (JSProgram [JSArrayExpression [JSNumberLiteral 1, JSNumberLiteral 2, JSNumberLiteral 3]])
    parse "[1, 2, [3, 4, 5]]" `shouldBe`
      Right (JSProgram [JSArrayExpression [JSNumberLiteral 1, JSNumberLiteral 2, JSArrayExpression [JSNumberLiteral 3, JSNumberLiteral 4, JSNumberLiteral 5]]])
    parse "[1, 2, 3][0]" `shouldBe`
      Right (JSProgram [JSMemberExpression (JSNumberLiteral 0) (JSArrayExpression [JSNumberLiteral 1,JSNumberLiteral 2,JSNumberLiteral 3])])
    parse "[1, 2, [3, 4, 5]][2][0]" `shouldBe`
      Right (JSProgram [JSMemberExpression (JSNumberLiteral 0) (JSMemberExpression (JSNumberLiteral 2) (JSArrayExpression [JSNumberLiteral 1,JSNumberLiteral 2,JSArrayExpression [JSNumberLiteral 3,JSNumberLiteral 4,JSNumberLiteral 5]]))])

logicalOperatorsSpec :: Spec
logicalOperatorsSpec =
  it "Logical Operators" $ do
    parse "1 && 2" `shouldBe`
      Right (JSProgram [JSBinaryExpression "&&" (JSNumberLiteral 1) (JSNumberLiteral 2)])
    parse "1 || 2" `shouldBe`
      Right (JSProgram [JSBinaryExpression "||" (JSNumberLiteral 1) (JSNumberLiteral 2)])
    parse "1 && 2 || 3" `shouldBe`
      Right (JSProgram [JSBinaryExpression "||" (JSBinaryExpression "&&" (JSNumberLiteral 1) (JSNumberLiteral 2)) (JSNumberLiteral 3)])

associativeArraySpec :: Spec
associativeArraySpec =
  it "Associative Array" $ do
    parse "{ x: 1 }" `shouldBe`
      Right (JSProgram [JSBlockStatement [JSLabeledStatement (JSIdentifier "x") (JSNumberLiteral 1)]])
    parse "{ x: 1, y: 2 }" `shouldBe`
      Right (JSProgram [JSBlockStatement [JSLabeledStatement (JSIdentifier "x") (JSNumberLiteral 1), JSLabeledStatement (JSIdentifier "y") (JSNumberLiteral 2)]])
    parse "{ x: 1, y: { z: 2 } }" `shouldBe`
      Right (JSProgram [JSBlockStatement [JSLabeledStatement (JSIdentifier "x") (JSNumberLiteral 1), JSLabeledStatement (JSIdentifier "y") (JSBlockStatement [JSLabeledStatement (JSIdentifier "z") (JSNumberLiteral 2)])]])
    parse "x.y" `shouldBe`
      Right (JSProgram [JSObjectMemberExpression (JSIdentifier "y") (JSIdentifier "x")])
    parse "x.y.z" `shouldBe`
      Right (JSProgram [JSObjectMemberExpression (JSIdentifier "z") (JSObjectMemberExpression (JSIdentifier "y") (JSIdentifier "x"))])

unaryOperatorsSpec :: Spec
unaryOperatorsSpec =
  it "Unary Operators" $ do
    parse "!1" `shouldBe`
      Right (JSProgram [JSUnaryExpression "!" (JSNumberLiteral 1)])
    parse "+1" `shouldBe`
      Right (JSProgram [JSUnaryExpression "+" (JSNumberLiteral 1)])
    parse "+1\n+1" `shouldBe`
      Right (JSProgram [JSBinaryExpression "+" (JSUnaryExpression "+" (JSNumberLiteral 1)) (JSNumberLiteral 1)])
    parse "+1;\n+1" `shouldBe`
      Right (JSProgram [JSUnaryExpression "+" (JSNumberLiteral 1), JSUnaryExpression "+" (JSNumberLiteral 1)])
    parse "-1" `shouldBe`
      Right (JSProgram [JSUnaryExpression "-" (JSNumberLiteral 1)])
    parse "-1\n-1" `shouldBe`
      Right (JSProgram [JSBinaryExpression "-" (JSUnaryExpression "-" (JSNumberLiteral 1)) (JSNumberLiteral 1)])
    parse "-1;\n-1" `shouldBe`
      Right (JSProgram [JSUnaryExpression "-" (JSNumberLiteral 1), JSUnaryExpression "-" (JSNumberLiteral 1)])
    parse "!1\n+1" `shouldBe`
      Right (JSProgram [JSBinaryExpression "+" (JSUnaryExpression "!" (JSNumberLiteral 1)) (JSNumberLiteral 1)])
    parse "!1;\n+1" `shouldBe`
      Right (JSProgram [JSUnaryExpression "!" (JSNumberLiteral 1), JSUnaryExpression "+" (JSNumberLiteral 1)])

bitwiseOperatorsSpec :: Spec
bitwiseOperatorsSpec =
  it "Bitwise Operators" $ do
    parse "1 & 1" `shouldBe`
      Right (JSProgram [JSBinaryExpression "&" (JSNumberLiteral 1) (JSNumberLiteral 1)])
    parse "1 | 1" `shouldBe`
      Right (JSProgram [JSBinaryExpression "|" (JSNumberLiteral 1) (JSNumberLiteral 1)])
    parse "1 << 1" `shouldBe`
      Right (JSProgram [JSBinaryExpression "<<" (JSNumberLiteral 1) (JSNumberLiteral 1)])
    parse "1 >> 1" `shouldBe`
      Right (JSProgram [JSBinaryExpression ">>" (JSNumberLiteral 1) (JSNumberLiteral 1)])

spec :: Spec
spec =
  describe "JavaScript" $ do
    commentSpec
    stringLiteralSpec
    arithmeticOperatorsSpec
    comparisonOperatorsSpec
    whileStatementSpec
    ifStatementSpec
    forStatementSpec
    tryCatchStatementSpec
    switchStatementSpec
    variableSpec
    functionStatementSpec
    arrayStatementSpec
    logicalOperatorsSpec
    associativeArraySpec
    unaryOperatorsSpec
    bitwiseOperatorsSpec
