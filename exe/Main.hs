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
evalClause _ [] = False
evalClause valuation (l:ls) = evalLiteral valuation l || evalClause valuation ls

evalFormula :: Valuation -> Formula -> Bool
evalFormula valuation [] = True
evalFormula valuation (c:cs) = evalClause valuation c && evalFormula valuation cs

main :: IO ()
main = hspec $ do
  describe "evaluator" $ do
    it "positive literal" $
      evalLiteral (const True) (Positive 0) `shouldBe` True
    it "negative literal" $
      evalLiteral (const True) (Negative 0) `shouldBe` False
