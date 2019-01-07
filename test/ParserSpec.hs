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
      Right (JSProgram [JSPlus (JSNumber 1) (JSNumber 1)])
    parse "1 + 1 /* Hello World */ 2 + 2" `shouldBe`
      Right (JSProgram [JSPlus (JSNumber 1) (JSNumber 1), JSPlus (JSNumber 2) (JSNumber 2)])
    parse "1 + 1 /* /* Hello World */ */ 2 + 2" `shouldBe`
      Right (JSProgram [JSPlus (JSNumber 1) (JSNumber 1), JSPlus (JSNumber 2) (JSNumber 2)])

stringLiteralSpec :: Spec
stringLiteralSpec =
  it "String Literal" $ do
    parse "\"a\"" `shouldBe`
      Right (JSProgram [JSString "a"])
    parse "\"abc\"" `shouldBe`
      Right (JSProgram [JSString "abc"])
    parse "\"\\\"abc\\\"\"" `shouldBe`
      Right (JSProgram [JSString "\\\"abc\\\""])
    parse "'a'" `shouldBe`
      Right (JSProgram [JSString "a"])
    parse "'abc'" `shouldBe`
      Right (JSProgram [JSString "abc"])
    parse "'\\'abc\\''" `shouldBe`
      Right (JSProgram [JSString "\\'abc\\'"])

arithmeticOperatorsSpec :: Spec
arithmeticOperatorsSpec =
  describe "Arithmetic operators" $ do
    it "Addition" $ do
      parse "1+1" `shouldBe`
        Right (JSProgram [JSPlus (JSNumber 1) (JSNumber 1)])
      parse "++1" `shouldBe`
        Right (JSProgram [JSPrefixPlusUpdate (JSNumber 1)])
      parse "++1;\n++1" `shouldBe`
        Right (JSProgram [JSPrefixPlusUpdate (JSNumber 1), JSPrefixPlusUpdate (JSNumber 1)])
      parse "1++" `shouldBe`
        Right (JSProgram [JSPostfixPlusUpdate (JSNumber 1)])
      parse "1++;\n1++" `shouldBe`
        Right (JSProgram [JSPostfixPlusUpdate (JSNumber 1), JSPostfixPlusUpdate (JSNumber 1)])
    it "Subtraction" $ do
      parse "1-1" `shouldBe`
        Right (JSProgram [JSMinus (JSNumber 1) (JSNumber 1)])
      parse "--1" `shouldBe`
        Right (JSProgram [JSPrefixMinusUpdate (JSNumber 1)])
      parse "--1;\n--1" `shouldBe`
        Right (JSProgram [JSPrefixMinusUpdate (JSNumber 1), JSPrefixMinusUpdate (JSNumber 1)])
      parse "1--" `shouldBe`
        Right (JSProgram [JSPostfixMinusUpdate (JSNumber 1)])
      parse "1--;\n1--" `shouldBe`
        Right (JSProgram [JSPostfixMinusUpdate (JSNumber 1), JSPostfixMinusUpdate (JSNumber 1)])
    it "Multiplication" $ do
      parse "1*1" `shouldBe`
        Right (JSProgram [JSTimes (JSNumber 1) (JSNumber 1)])
    it "Division" $ do
      parse "1/1" `shouldBe`
        Right (JSProgram [JSDivide (JSNumber 1) (JSNumber 1)])
    it "Remainder" $ do
      parse "1%1" `shouldBe`
        Right (JSProgram [JSModulo (JSNumber 1) (JSNumber 1)])
    it "Exponentiation" $ do
      parse "1**1" `shouldBe`
        Right (JSProgram [JSExponentiation (JSNumber 1) (JSNumber 1)])
    it "Parentheses" $ do
      parse "(1+2)*3" `shouldBe`
        Right (JSProgram [JSTimes (JSPlus (JSNumber 1) (JSNumber 2)) (JSNumber 3)])
      parse "(1+2)/3" `shouldBe`
        Right (JSProgram [JSDivide (JSPlus (JSNumber 1) (JSNumber 2)) (JSNumber 3)])

comparisonOperatorsSpec :: Spec
comparisonOperatorsSpec =
  describe "Comparison operators" $ do
    it "Equality" $ do
      parse "1==1" `shouldBe`
        Right (JSProgram [JSLooseEqual (JSNumber 1) (JSNumber 1)])
    it "Inequality" $ do
      parse "1!=1" `shouldBe`
        Right (JSProgram [JSLooseNotEqual (JSNumber 1) (JSNumber 1)])
    it "Strict Equality" $ do
      parse "1===1" `shouldBe`
        Right (JSProgram [JSStrictEqual (JSNumber 1) (JSNumber 1)])
    it "Strict Inequality" $ do
      parse "1!==1" `shouldBe`
        Right (JSProgram [JSStrictNotEqual (JSNumber 1) (JSNumber 1)])
    it "Greater than operator" $ do
      parse "1>1" `shouldBe`
        Right (JSProgram [JSGreater (JSNumber 1) (JSNumber 1)])
    it "Greater than or equal operator" $ do
      parse "1>=1" `shouldBe`
        Right (JSProgram [JSGreaterOrEqual (JSNumber 1) (JSNumber 1)])
    it "Less than operator" $ do
      parse "1<1" `shouldBe`
        Right (JSProgram [JSLess (JSNumber 1) (JSNumber 1)])
    it "Less than or equal operator" $ do
      parse "1<=1" `shouldBe`
        Right (JSProgram [JSLessOrEqual (JSNumber 1) (JSNumber 1)])

whileStatementSpec :: Spec
whileStatementSpec =
  it "While Statement" $ do
    parse "while (true) { 1 + 1 }" `shouldBe`
      Right (JSProgram [JSWhile (JSBoolean True) (JSBlock [JSPlus (JSNumber 1) (JSNumber 1)])])
    parse "while (true) { continue }" `shouldBe`
      Right (JSProgram [JSWhile (JSBoolean True) (JSBlock [JSContinue])])
    parse "while (true) { break }" `shouldBe`
      Right (JSProgram [JSWhile (JSBoolean True) (JSBlock [JSBreak])])
    parse "while (true) 1 + 1" `shouldBe`
      Right (JSProgram [JSWhile (JSBoolean True) (JSPlus (JSNumber 1) (JSNumber 1))])

ifStatementSpec :: Spec
ifStatementSpec =
  it "If Statement" $ do
    parse "if (true) { 1 + 1 } else { 2 + 2 }" `shouldBe`
      Right (JSProgram [JSIf (JSBoolean True) (JSBlock [JSPlus (JSNumber 1) (JSNumber 1)]) (Just (JSBlock [JSPlus (JSNumber 2) (JSNumber 2)]))])
    parse "if (true) { 1 + 1 } else if (true) { 2 + 2 }" `shouldBe`
      Right (JSProgram [JSIf (JSBoolean True) (JSBlock [JSPlus (JSNumber 1) (JSNumber 1)]) (Just (JSIf (JSBoolean True) (JSBlock [JSPlus (JSNumber 2) (JSNumber 2)]) Nothing))])
    parse "if (true) { 1 + 1 } else if (true) { 2 + 2 } else { 3 + 3 }" `shouldBe`
      Right (JSProgram [JSIf (JSBoolean True) (JSBlock [JSPlus (JSNumber 1) (JSNumber 1)]) (Just (JSIf (JSBoolean True) (JSBlock [JSPlus (JSNumber 2) (JSNumber 2)]) (Just (JSBlock [JSPlus (JSNumber 3) (JSNumber 3)]))))])

forStatementSpec :: Spec
forStatementSpec =
  it "For Statement" $ do
    parse "for (true; true; true) { 1 + 1 }" `shouldBe`
      Right (JSProgram [JSFor (JSBoolean True, JSBoolean True, JSBoolean True) (JSBlock [JSPlus (JSNumber 1) (JSNumber 1)])])
    parse "for (;;) { 1 + 1 }" `shouldBe`
      Right (JSProgram [JSFor (JSEmpty, JSEmpty, JSEmpty) (JSBlock [JSPlus (JSNumber 1) (JSNumber 1)])])

tryCatchStatementSpec :: Spec
tryCatchStatementSpec =
  it "Try Catch Statement" $ do
    parse "try { 1 + 1 } catch () { 2 + 2 }" `shouldBe`
      Right (JSProgram [JSTryCatch (JSBlock [JSPlus (JSNumber 1) (JSNumber 1)]) JSEmpty (JSBlock [JSPlus (JSNumber 2) (JSNumber 2)]) Nothing])
    parse "try { 1 + 1 } catch () { 2 + 2 } finally { 3 + 3 }" `shouldBe`
      Right (JSProgram [JSTryCatch (JSBlock [JSPlus (JSNumber 1) (JSNumber 1)]) JSEmpty (JSBlock [JSPlus (JSNumber 2) (JSNumber 2)]) (Just (JSBlock [JSPlus (JSNumber 3) (JSNumber 3)]))])

switchStatementSpec :: Spec
switchStatementSpec =
  it "Switch Statement" $ do
    parse "switch(true) { case true: 1 + 1; case false: 2 + 2; }" `shouldBe`
      Right (JSProgram [JSSwitch (JSBoolean True) (JSBlock [JSCase (JSBoolean True) (JSBlock [JSPlus (JSNumber 1) (JSNumber 1)]),JSCase (JSBoolean False) (JSBlock [JSPlus (JSNumber 2) (JSNumber 2)])])])
    parse "switch(true) { case true: 1 + 1; case false: 2 + 2; default: 3 + 3; }" `shouldBe`
      Right (JSProgram [JSSwitch (JSBoolean True) (JSBlock [JSCase (JSBoolean True) (JSBlock [JSPlus (JSNumber 1) (JSNumber 1)]),JSCase (JSBoolean False) (JSBlock [JSPlus (JSNumber 2) (JSNumber 2)]),JSDefault (JSBlock [JSPlus (JSNumber 3) (JSNumber 3)])])])

variableSpec :: Spec
variableSpec =
  it "Variable" $ do
    parse "x = 1" `shouldBe`
      Right (JSProgram [JSAssignment (JSIdentifier "x") (JSNumber 1)])
    parse "var x = 1" `shouldBe`
      Right (JSProgram [JSAssignment (JSVariableDeclaration (JSIdentifier "x")) (JSNumber 1)])

functionStatementSpec :: Spec
functionStatementSpec =
  it "Function Statement" $ do
    parse "function f() {}" `shouldBe`
      Right (JSProgram [JSFunctionDeclaration (JSIdentifier "f") [] (JSBlock [])])
    parse "function f(x) {}" `shouldBe`
      Right (JSProgram [JSFunctionDeclaration (JSIdentifier "f") [JSIdentifier "x"] (JSBlock [])])
    parse "function f(x, y) {}" `shouldBe`
      Right (JSProgram [JSFunctionDeclaration (JSIdentifier "f") [JSIdentifier "x", JSIdentifier "y"] (JSBlock [])])
    parse "function f(x, y) { return x + y }" `shouldBe`
      Right (JSProgram [JSFunctionDeclaration (JSIdentifier "f") [JSIdentifier "x",JSIdentifier "y"] (JSBlock [JSReturn (JSPlus (JSIdentifier "x") (JSIdentifier "y"))])])
    parse "f()" `shouldBe`
      Right (JSProgram [JSCall [] (JSIdentifier "f")])
    parse "f(1, 2)" `shouldBe`
      Right (JSProgram [JSCall [JSNumber 1,JSNumber 2] (JSIdentifier "f")])
    parse "f(1, 2)(3, 4)" `shouldBe`
      Right (JSProgram [JSCall [JSNumber 3,JSNumber 4] (JSCall [JSNumber 1,JSNumber 2] (JSIdentifier "f"))])

arrayStatementSpec :: Spec
arrayStatementSpec =
  it "Array Statement" $ do
    parse "[]" `shouldBe`
      Right (JSProgram [JSArray []])
    parse "[1]" `shouldBe`
      Right (JSProgram [JSArray [JSNumber 1]])
    parse "[1, 2, 3]" `shouldBe`
      Right (JSProgram [JSArray [JSNumber 1, JSNumber 2, JSNumber 3]])
    parse "[1, 2, [3, 4, 5]]" `shouldBe`
      Right (JSProgram [JSArray [JSNumber 1, JSNumber 2, JSArray [JSNumber 3, JSNumber 4, JSNumber 5]]])
    parse "[1, 2, 3][0]" `shouldBe`
      Right (JSProgram [JSMember (JSNumber 0) (JSArray [JSNumber 1,JSNumber 2,JSNumber 3])])
    parse "[1, 2, [3, 4, 5]][2][0]" `shouldBe`
      Right (JSProgram [JSMember (JSNumber 0) (JSMember (JSNumber 2) (JSArray [JSNumber 1,JSNumber 2,JSArray [JSNumber 3,JSNumber 4,JSNumber 5]]))])

logicalOperatorsSpec :: Spec
logicalOperatorsSpec =
  it "Logical Operators" $ do
    parse "1 && 2" `shouldBe`
      Right (JSProgram [JSAndLogical (JSNumber 1) (JSNumber 2)])
    parse "1 || 2" `shouldBe`
      Right (JSProgram [JSOrLogical (JSNumber 1) (JSNumber 2)])
    parse "1 && 2 || 3" `shouldBe`
      Right (JSProgram [JSOrLogical (JSAndLogical (JSNumber 1) (JSNumber 2)) (JSNumber 3)])

associativeArraySpec :: Spec
associativeArraySpec =
  it "Associative Array" $ do
    parse "{ x: 1 }" `shouldBe`
      Right (JSProgram [JSBlock [JSLabeled (JSIdentifier "x") (JSNumber 1)]])
    parse "{ x: 1, y: 2 }" `shouldBe`
      Right (JSProgram [JSBlock [JSLabeled (JSIdentifier "x") (JSNumber 1), JSLabeled (JSIdentifier "y") (JSNumber 2)]])
    parse "{ x: 1, y: { z: 2 } }" `shouldBe`
      Right (JSProgram [JSBlock [JSLabeled (JSIdentifier "x") (JSNumber 1), JSLabeled (JSIdentifier "y") (JSBlock [JSLabeled (JSIdentifier "z") (JSNumber 2)])]])
    parse "x.y" `shouldBe`
      Right (JSProgram [JSMember (JSIdentifier "y") (JSIdentifier "x")])
    parse "x.y.z" `shouldBe`
      Right (JSProgram [JSMember (JSIdentifier "z") (JSMember (JSIdentifier "y") (JSIdentifier "x"))])

unaryOperatorsSpec :: Spec
unaryOperatorsSpec =
  it "Unary Operators" $ do
    parse "!1" `shouldBe`
      Right (JSProgram [JSPrefixNot (JSNumber 1)])
    parse "+1" `shouldBe`
      Right (JSProgram [JSPrefixPlus (JSNumber 1)])
    parse "+1\n+1" `shouldBe`
      Right (JSProgram [JSPlus (JSPrefixPlus (JSNumber 1)) (JSNumber 1)])
    parse "+1;\n+1" `shouldBe`
      Right (JSProgram [JSPrefixPlus (JSNumber 1), JSPrefixPlus (JSNumber 1)])
    parse "-1" `shouldBe`
      Right (JSProgram [JSPrefixMinus (JSNumber 1)])
    parse "-1\n-1" `shouldBe`
      Right (JSProgram [JSMinus (JSPrefixMinus (JSNumber 1)) (JSNumber 1)])
    parse "-1;\n-1" `shouldBe`
      Right (JSProgram [JSPrefixMinus (JSNumber 1), JSPrefixMinus (JSNumber 1)])
    parse "!1\n+1" `shouldBe`
      Right (JSProgram [JSPlus (JSPrefixNot (JSNumber 1)) (JSNumber 1)])
    parse "!1;\n+1" `shouldBe`
      Right (JSProgram [JSPrefixNot (JSNumber 1), JSPrefixPlus (JSNumber 1)])

bigIntegerSpec :: Spec
bigIntegerSpec =
  it "BigInt" $ do
    parse "1n" `shouldBe`
      Right (JSProgram [JSBigInt 1])
    parse "1n\n1n" `shouldBe`
      Right (JSProgram [JSBigInt 1, JSBigInt 1])

bitwiseOperatorsSpec :: Spec
bitwiseOperatorsSpec =
  it "Bitwise Operators" $ do
    parse "1 & 1" `shouldBe`
      Right (JSProgram [JSAnd (JSNumber 1) (JSNumber 1)])
    parse "1 | 1" `shouldBe`
      Right (JSProgram [JSOr (JSNumber 1) (JSNumber 1)])
    parse "1 << 1" `shouldBe`
      Right (JSProgram [JSLeftShift (JSNumber 1) (JSNumber 1)])
    parse "1 >> 1" `shouldBe`
      Right (JSProgram [JSRightShift (JSNumber 1) (JSNumber 1)])

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
    bigIntegerSpec
    bitwiseOperatorsSpec
