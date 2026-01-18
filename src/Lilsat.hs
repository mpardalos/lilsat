{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
    VariableData (..),
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

data Reason
  = Decision {level :: Int}
  | Implied {level :: Int, antecedent :: Int}
  deriving (Show, Eq)

instance Ord Reason where
  compare (Decision level1) (Decision level2) =
    compare level1 level2
  compare (Implied level1 antecedent1) (Implied level2 antecedent2) =
    compare level1 level2 <> compare antecedent1 antecedent2
  compare (Decision level1) (Implied level2 _) =
    compare level1 level2 <> LT
  compare (Implied level1 _) (Decision level2) =
    compare level1 level2 <> GT

data VariableData = VariableData
  { value :: Bool,
    reason :: Reason
  }
  deriving (Show)

type Valuation = IntMap VariableData

learn :: Literal -> Reason -> Valuation -> Valuation
learn (Literal lit) reason valuation
  | IntMap.member lit valuation = error ("Double learn " ++ show lit)
  | otherwise = IntMap.insert (abs lit) (VariableData {value = lit > 0, reason}) valuation

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
evalLiteral valuation (Literal lit) =
  let atomValue = value <$> IntMap.lookup (abs lit) valuation
   in if lit > 0 then atomValue else not <$> atomValue

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

type ClauseIdx = Int

unitPropagate :: Formula -> Valuation -> Either ClauseIdx Valuation
unitPropagate formula initialValuation = V.foldM unitPropagateClause initialValuation $ V.imap (,) formula
  where
    literalLevel :: Valuation -> Literal -> Int
    literalLevel v (Literal lit) = case IntMap.lookup (abs lit) v of
      Just varData -> varData.reason.level
      Nothing -> -1

    unitPropagateClause :: Valuation -> (Int, Clause) -> Either ClauseIdx Valuation
    unitPropagateClause v (idx, clause) =
      case decideClause v clause of
        ClauseSAT -> Right v
        ClauseUnresolved -> Right v
        ClauseUNSAT -> Left idx
        ClauseUnit lit -> Right (learn lit (Implied {antecedent = idx, level = maximum (V.map (literalLevel v) clause)}) v)

checkSat :: Formula -> Answer
checkSat formula = go 0 IntMap.empty
  where
    go :: Int -> Valuation -> Answer
    go decisionLevel valuation
      | Just lit <- chooseLit valuation formula =
          let withLit =
                go (decisionLevel + 1)
                  <$> unitPropagate formula (learn lit (Decision decisionLevel) valuation)
              withNegation =
                go (decisionLevel + 1)
                  <$> unitPropagate formula (learn (negateLit lit) (Decision decisionLevel) valuation)
           in case withLit of
                Right answer@SAT {} -> answer
                _ -> case withNegation of
                  Right answer@SAT {} -> answer
                  _ -> UNSAT
      | otherwise = SAT valuation
