-- HSpec tests for Eval.hs
-- Execute: runhaskell EvalSpec.hs

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Val
import Eval

main :: IO ()
main = hspec $ do

  describe "eval" $ do
    context "*" $ do
        it "multiplies integers" $ do
            eval "*" [Integer 2, Integer 3] `shouldBe` Right [Integer 6]
        
        it "multiplies floats" $ do
            eval "*" [Integer 2, Real 3.0] `shouldBe` Right [Real 6.0]
            eval "*" [Real 3.0, Integer 3] `shouldBe` Right [Real 9.0]
            eval "*" [Real 4.0, Real 3.0] `shouldBe` Right [Real 12.0]

        it "errors on too few arguments" $ do   
            eval "*" [] `shouldBe` Left "Stack underflow"
            eval "*" [Integer 2] `shouldBe` Left "Stack underflow"

    context "DUP" $ do
        it "duplicates values" $ do
            eval "DUP" [Integer 2] `shouldBe` Right [Integer 2, Integer 2]
            eval "DUP" [Real 2.2] `shouldBe` Right [Real 2.2, Real 2.2]
            eval "DUP" [Id "x"] `shouldBe` Right [Id "x", Id "x"]

        it "errors on empty stack" $ do
            eval "DUP" [] `shouldBe` Left "Stack underflow"

  describe "evalOut" $ do
      context "." $ do
        it "prints top of stack" $ do
            evalOut "." ([Id "x"], "") `shouldBe` Right ([], "x")
            evalOut "." ([Integer 2], "") `shouldBe` Right ([], "2")
            evalOut "." ([Real 2.2], "") `shouldBe` Right ([], "2.2") 

        it "errors on empty stack" $ do
            evalOut "." ([], "") `shouldBe` Left "Stack underflow"

      it "eval pass-through" $ do
         evalOut "*" ([Real 2.0, Integer 2], "blah") `shouldBe` Right ([Real 4.0], "blah") 

  describe "Eval.hs built-in function tests" $ do
    it "adds two numbers" $ do
      eval "+" [Integer 2, Integer 3] `shouldBe` Right [Integer 5]

    it "subtracts two numbers" $ do
      eval "-" [Integer 2, Integer 7] `shouldBe` Right [Integer 5]

    it "divides two numbers" $ do
      eval "/" [Integer 10, Integer 2] `shouldBe` Right [Integer 0]

    it "returns error on division by zero" $ do
      eval "/" [Integer 0, Integer 5] `shouldBe` Left "Division by zero error"

    it "calculates power" $ do
      eval "^" [Integer 2, Integer 3] `shouldBe` Right [Integer 9]

    it "converts value to string" $ do
      eval "STR" [Integer 123] `shouldBe` Right [Id "123"]

    it "concatenates two strings" $ do
      eval "CONCAT2" [Id "World", Id "Hello"] `shouldBe` Right [Id "HelloWorld"]

    it "concatenates three strings" $ do
      eval "CONCAT3" [Id "!", Id "World", Id "Hello"] `shouldBe` Right [Id "HelloWorld!"]
