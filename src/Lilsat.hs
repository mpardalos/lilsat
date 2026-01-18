{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Lilsat
  ( -- Types
    Atom,
    Literal (..),
    Clause,
    Formula,
    Valuation,
    Answer (..),
    Reason (..),
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
import Data.Function ((&))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Safe (headMay, readNote)

type Atom = Int

newtype Literal = Literal Atom
  deriving (Eq, Ord)

instance Show Literal where
  show (Literal lit)
    | lit < 0 = "¬" ++ show (abs lit)
    | otherwise = show lit

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

data Reason = Decision | Consequence Int
  deriving (Show)

type Valuation = IntMap Reason

learn :: Literal -> Reason -> Valuation -> Valuation
learn (Literal lit) reason valuation
  | IntMap.member lit valuation = error ("Double learn " ++ show lit)
  | IntMap.member (-lit) valuation = error ("Conflicting learn " ++ show lit)
  | otherwise = IntMap.insert lit reason valuation

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
  | IntMap.member lit valuation = Just True
  | IntMap.member (-lit) valuation = Just False
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

data ClauseDecision
  = ClauseSAT
  | ClauseUNSAT
  | ClauseUnresolved
  | ClauseUnit Literal

decideClause :: Valuation -> Clause -> ClauseDecision
decideClause v = V.foldr go ClauseUNSAT
  where
    go :: Literal -> ClauseDecision -> ClauseDecision
    go lit decision = case (decision, evalLiteral v lit) of
      (_, Just True) -> ClauseSAT
      (_, Just False) -> decision
      (ClauseSAT, _) -> ClauseSAT
      (ClauseUNSAT, Nothing) -> ClauseUnit lit
      (ClauseUnresolved, _) -> ClauseUnresolved
      (ClauseUnit _, Nothing) -> ClauseUnresolved

unitPropagate :: Formula -> Valuation -> Maybe Valuation
unitPropagate formula initialValuation = V.foldM unitPropagateClause initialValuation $ V.imap (,) formula
  where
    unitPropagateClause :: Valuation -> (Int, Clause) -> Maybe Valuation
    unitPropagateClause v (idx, clause) =
      case decideClause v clause of
        ClauseSAT -> Just v
        ClauseUnresolved -> Just v
        ClauseUNSAT -> Nothing
        ClauseUnit lit -> Just (learn lit (Consequence idx) v)

checkSat :: Formula -> Answer
checkSat formula = go IntMap.empty
  where
    go :: Valuation -> Answer
    go valuation
      | Just lit <- chooseLit valuation formula =
          let withLit = go <$> unitPropagate formula (learn lit Decision valuation)
              withNegation = go <$> unitPropagate formula (learn (negateLit lit) Decision valuation)
           in case withLit of
                Just answer@SAT {} -> answer
                _ -> fromMaybe UNSAT withNegation
      | otherwise = SAT valuation
