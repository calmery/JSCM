module ParserSpec (spec) where

import           Parser     (Expression (..), parse)
import           RIO
import           Test.Hspec

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

spec :: Spec
spec =
  describe "JavaScript" $ do
    arithmeticOperatorsSpec
    comparisonOperatorsSpec
    whileStatementSpec
