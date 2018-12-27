module ParserSpec (spec) where

import           Parser     (Expression (..), parse)
import           RIO
import           Test.Hspec

arithmeticOperatorsSpec :: Spec
arithmeticOperatorsSpec =
  describe "Arithmetic operators" $ do
    it "Addition" $ do
      parse "1+1" `shouldBe`
        Right (JSPlus (JSNumber 1) (JSNumber 1))
    it "Subtraction" $ do
      parse "1-1" `shouldBe`
        Right (JSMinus (JSNumber 1) (JSNumber 1))
    it "Multiplication" $ do
      parse "1*1" `shouldBe`
        Right (JSTimes (JSNumber 1) (JSNumber 1))
    it "Division" $ do
      parse "1/1" `shouldBe`
        Right (JSDivide (JSNumber 1) (JSNumber 1))
    it "Remainder" $ do
      parse "1%1" `shouldBe`
        Right (JSModulo (JSNumber 1) (JSNumber 1))

comparisonOperatorsSpec :: Spec
comparisonOperatorsSpec =
  describe "Comparison operators" $ do
    it "Equality" $ do
      parse "1==1" `shouldBe`
        Right (JSLooseEqual (JSNumber 1) (JSNumber 1))
    it "Inequality" $ do
      parse "1!=1" `shouldBe`
        Right (JSLooseNotEqual (JSNumber 1) (JSNumber 1))
    it "Strict Equality" $ do
      parse "1===1" `shouldBe`
        Right (JSStrictEqual (JSNumber 1) (JSNumber 1))
    it "Strict Inequality" $ do
      parse "1!==1" `shouldBe`
        Right (JSStrictNotEqual (JSNumber 1) (JSNumber 1))
    it "Greater than operator" $ do
      parse "1>1" `shouldBe`
        Right (JSGreater (JSNumber 1) (JSNumber 1))
    it "Greater than or equal operator" $ do
      parse "1>=1" `shouldBe`
        Right (JSGreaterOrEqual (JSNumber 1) (JSNumber 1))
    it "Less than operator" $ do
      parse "1<1" `shouldBe`
        Right (JSLess (JSNumber 1) (JSNumber 1))
    it "Less than or equal operator" $ do
      parse "1<=1" `shouldBe`
        Right (JSLessOrEqual (JSNumber 1) (JSNumber 1))

spec :: Spec
spec =
  describe "JavaScript" $ do
    arithmeticOperatorsSpec
    comparisonOperatorsSpec
