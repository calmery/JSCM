module ParserSpec (spec) where

import           Parser     (Expression (..), parse)
import           RIO
import           Test.Hspec

arithmeticOperatorsSpec :: Spec
arithmeticOperatorsSpec =
  describe "Arithmetic operators" $ do
    it "Addition" $ do
      parse "1+1" `shouldBe`
        Right (JSBody [JSPlus (JSNumber 1) (JSNumber 1)])
    it "Subtraction" $ do
      parse "1-1" `shouldBe`
        Right (JSBody [JSMinus (JSNumber 1) (JSNumber 1)])
    it "Multiplication" $ do
      parse "1*1" `shouldBe`
        Right (JSBody [JSTimes (JSNumber 1) (JSNumber 1)])
    it "Division" $ do
      parse "1/1" `shouldBe`
        Right (JSBody [JSDivide (JSNumber 1) (JSNumber 1)])
    it "Remainder" $ do
      parse "1%1" `shouldBe`
        Right (JSBody [JSModulo (JSNumber 1) (JSNumber 1)])
    it "Exponentiation" $ do
      parse "1**1" `shouldBe`
        Right (JSBody [JSExponentiation (JSNumber 1) (JSNumber 1)])

comparisonOperatorsSpec :: Spec
comparisonOperatorsSpec =
  describe "Comparison operators" $ do
    it "Equality" $ do
      parse "1==1" `shouldBe`
        Right (JSBody [JSLooseEqual (JSNumber 1) (JSNumber 1)])
    it "Inequality" $ do
      parse "1!=1" `shouldBe`
        Right (JSBody [JSLooseNotEqual (JSNumber 1) (JSNumber 1)])
    it "Strict Equality" $ do
      parse "1===1" `shouldBe`
        Right (JSBody [JSStrictEqual (JSNumber 1) (JSNumber 1)])
    it "Strict Inequality" $ do
      parse "1!==1" `shouldBe`
        Right (JSBody [JSStrictNotEqual (JSNumber 1) (JSNumber 1)])
    it "Greater than operator" $ do
      parse "1>1" `shouldBe`
        Right (JSBody [JSGreater (JSNumber 1) (JSNumber 1)])
    it "Greater than or equal operator" $ do
      parse "1>=1" `shouldBe`
        Right (JSBody [JSGreaterOrEqual (JSNumber 1) (JSNumber 1)])
    it "Less than operator" $ do
      parse "1<1" `shouldBe`
        Right (JSBody [JSLess (JSNumber 1) (JSNumber 1)])
    it "Less than or equal operator" $ do
      parse "1<=1" `shouldBe`
        Right (JSBody [JSLessOrEqual (JSNumber 1) (JSNumber 1)])

spec :: Spec
spec =
  describe "JavaScript" $ do
    arithmeticOperatorsSpec
    comparisonOperatorsSpec
