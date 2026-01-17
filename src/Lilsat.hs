{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Lilsat
  ( -- Types
    Atom,
    Literal,
    Clause,
    Formula,
    Valuation,
    Answer (..),
    -- Pattern synonyms
    pattern Positive,
    pattern Negative,
    -- Functions
    negateLit,
    isSAT,
    evalLiteral,
    evalClause,
    evalFormula,
    readCNF,
    checkSat,
  )
where

import Control.Monad (join)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Debug.Trace (trace)
import Safe (headMay, headNote, readNote)

type Atom = Int

newtype Literal = Literal Atom
  deriving (Show, Eq, Ord)

type Clause = Vector Literal

type Formula = Vector Clause

pattern Positive :: Atom -> Literal
pattern Positive n <- Literal (\x -> if x > 0 then Just (fromIntegral x) else Nothing -> Just n)
  where
    Positive n = Literal (fromIntegral n)

pattern Negative :: Atom -> Literal
pattern Negative n <- Literal (\x -> if x < 0 then Just (fromIntegral (negate x)) else Nothing -> Just n)
  where
    Negative n = Literal (fromIntegral (negate n))

{-# COMPLETE Positive, Negative #-}

negateLit :: Literal -> Literal
negateLit (Literal n) = Literal (-n)

type Valuation = IntSet

learn :: Literal -> Valuation -> Valuation
learn (Literal lit) valuation
  -- | trace ("Propagated " ++ show lit) False = undefined
  | IntSet.member (-lit) valuation = error ("Conflicting learn " ++ show lit)
  | otherwise = IntSet.insert lit valuation

learnNew :: Literal -> Valuation -> Valuation
learnNew (Literal lit) valuation
  -- | trace ("Decided " ++ show lit) False = undefined
  | IntSet.member lit valuation = trace ("Double learning " ++ show lit) valuation
  | IntSet.member (-lit) valuation = error ("Conflicting learn " ++ show lit)
  | otherwise = IntSet.insert lit valuation

data Answer
  = SAT Valuation
  | UNSAT
  deriving (Show)

isSAT :: Answer -> Bool
isSAT (SAT _) = True
isSAT _ = False

partialAnd :: Maybe Bool -> Maybe Bool -> Maybe Bool
partialAnd (Just False) _ = Just False
partialAnd _ (Just False) = Just False
partialAnd (Just b1) (Just b2) = Just (b1 && b2)
partialAnd _ _ = Nothing

partialOr :: Maybe Bool -> Maybe Bool -> Maybe Bool
partialOr (Just True) _ = Just True
partialOr _ (Just True) = Just True
partialOr (Just b1) (Just b2) = Just (b1 || b2)
partialOr _ _ = Nothing

evalLiteral :: Valuation -> Literal -> Maybe Bool
evalLiteral valuation (Literal lit)
  | IntSet.member lit valuation = Just True
  | IntSet.member (-lit) valuation = Just False
  | otherwise = Nothing -- error ("Not in valuation: " ++ show lit)

evalClause :: Valuation -> Clause -> Maybe Bool
evalClause valuation =
  V.foldr partialOr (Just False) . V.map (evalLiteral valuation)

evalFormula :: Valuation -> Formula -> Maybe Bool
evalFormula valuation =
  V.foldr partialAnd (Just True) . V.map (evalClause valuation)

readCNF :: Text -> Formula
readCNF txt =
  T.lines txt
    & map T.strip
    & filter (not . T.null)
    & filter (not . ("p" `T.isPrefixOf`))
    & filter (not . ("c" `T.isPrefixOf`))
    & filter (/= "%")
    & filter (/= "0")
    & map readClauseLine
    & V.fromList
  where
    readClauseLine :: Text -> Clause
    readClauseLine = V.fromList . readClause . map (readNote "literal" . T.unpack) . T.words

    readClause :: [Int] -> [Literal]
    readClause [] = error "Empty clause"
    readClause [0] = []
    readClause (0 : _) = error "Clause does not terminate after 0"
    readClause (x : xs) = Literal x : readClause xs

chooseLit :: Valuation -> Formula -> Maybe Literal
chooseLit valuation =
  headMay
    . V.toList
    . V.filter ((Nothing ==) . evalLiteral valuation)
    . join

unitPropagate :: Formula -> Valuation -> Maybe Valuation
unitPropagate formula initialValuation = V.foldM unitPropagateClause initialValuation formula
  where
    unitPropagateClause :: Valuation -> Clause -> Maybe Valuation
    unitPropagateClause valuation clause =
      let ambiguous = V.filter ((== Nothing) . evalLiteral valuation) clause
          hasTrue = (> 0) . V.length . V.filter ((== Just True) . evalLiteral valuation) $ clause
       in if hasTrue
            then Just valuation
            else case V.length ambiguous of
              0 -> Nothing
              1 -> Just (learn (V.head ambiguous) valuation)
              _ -> Just valuation

checkSat :: Formula -> Answer
checkSat formula = go IntSet.empty
  where
    go :: Valuation -> Answer
    go valuation
      | Just lit <- chooseLit valuation formula =
          let withLit = go <$> unitPropagate formula (learnNew lit valuation)
              withNegation = go <$> unitPropagate formula (learnNew (negateLit lit) valuation)
           in case withLit of
                Just answer@SAT {} -> answer
                _ -> fromMaybe UNSAT withNegation
      | otherwise = SAT valuation
