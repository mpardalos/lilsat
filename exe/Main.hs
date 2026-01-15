module Main where

import Test.Hspec

type Atom = Int
data Literal = Positive !Atom | Negative !Atom
type Clause = [Literal]
type Formula = [Clause]

type Valuation = Atom -> Bool

evalLiteral :: Valuation -> Literal -> Bool
evalLiteral valuation (Positive n) = valuation n
evalLiteral valuation (Negative n) = not (valuation n)

evalClause :: Valuation -> Clause -> Bool
evalClause valuation = any (evalLiteral valuation)

evalFormula :: Valuation -> Formula -> Bool
evalFormula valuation = all (evalClause valuation)

main :: IO ()
main = hspec $ do
  describe "evaluator" $ do
    describe "literal" $ do
      it "positive literal" $
        evalLiteral (const True) (Positive 0) `shouldBe` True
      it "negative literal" $
        evalLiteral (const True) (Negative 0) `shouldBe` False
    describe "clause" $ do
      it "yesno" $ evalClause (const True) [Positive 0, Negative 0] `shouldBe` True
      it "nono" $ evalClause (const True) [Negative 0, Negative 0] `shouldBe` False
    describe "formula" $ do
      it "yesno" $
        evalFormula (const True) [[Positive 0], [Negative 0]] `shouldBe` False
      it "yesno" $
        evalFormula (const True) [[Negative 0], [Negative 0]] `shouldBe` False
      it "yesyes" $
        evalFormula (const True) [[Positive 0], [Positive 0]] `shouldBe` True
