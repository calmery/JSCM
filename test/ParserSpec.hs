module ParserSpec (spec) where

import           Parser     (Expression (..), parse)
import           RIO
import           Test.Hspec

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
    it "Subtraction" $ do
      parse "1-1" `shouldBe`
        Right (JSProgram [JSMinus (JSNumber 1) (JSNumber 1)])
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
