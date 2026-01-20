{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

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
import Data.List (intercalate)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Safe (fromJustNote, headMay, readNote)
import Safe.Foldable (maximumNote, minimumNote)
import Text.Printf (printf)
import Data.Maybe (isJust)

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

type Valuation = Vector (Maybe VariableData)

learn :: Literal -> Reason -> Valuation -> Valuation
learn (Literal lit) reason valuation
  | isJust (valuation V.! abs lit) = error ("Double learn " ++ show lit)
  | otherwise = V.update valuation [(abs lit, Just VariableData {value = lit > 0, reason})]

varData :: Valuation -> Atom -> Maybe VariableData
varData v var
  | var < 0 = error "Negative var in varData"
  | otherwise = v V.! var

varLevel :: Valuation -> Atom -> Maybe Int
varLevel v var = case varData v var of
  Just d -> Just d.reason.level
  Nothing -> Nothing

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
  let atomValue = value <$> valuation V.! abs lit
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

atom :: Literal -> Atom
atom (Literal lit) = abs lit

-- Assuming that there exists a literal x, such that x ∈ ω1 and ¬x ∈ ω2, return a clause:
-- ω3 = { y | (y ∈ ω1 ∨ y ∈ ω2) ∧ (x ≠ y) }
resolveClauses :: Clause -> Clause -> Clause
resolveClauses c1 c2 = V.filter (not . common) (c1 V.++ c2)
  where
    common :: Literal -> Bool
    common lit =
      (V.elem lit c1 && V.elem (negateLit lit) c2)
        || (V.elem (negateLit lit) c1 && V.elem lit c2)

simplifyClause :: Clause -> Clause
simplifyClause = V.fromList . Set.toList . Set.fromList . V.toList

choosePivot1UIP :: Valuation -> Int -> Clause -> Maybe ClauseIdx
choosePivot1UIP v currentLevel clause =
  let atCurrentLevel =
        V.filter
          (maybe False ((== currentLevel) . level . reason) . varData v . atom)
          clause
      candidates =
        V.mapMaybe
          ( \lit -> case varData v (atom lit) of
              Just VariableData {reason = Implied {level, antecedent}}
                | level == currentLevel -> Just antecedent
              _ -> Nothing
          )
          atCurrentLevel
   in case V.length atCurrentLevel of
        0 -> error "No available pivot"
        1 -> Nothing -- We're at 1UIP
        _ -> headMay $ V.toList candidates

showBool :: Bool -> String
showBool True = "⊤"
showBool False = "⊥"

showClauseWith :: Valuation -> Clause -> String
showClauseWith v c =
  intercalate
    " ∨ "
    [ printf
        "%s (%s)"
        (show lit)
        ( case varData v (atom lit) of
            Nothing -> "undecided"
            Just VariableData {value, reason = Decision {level}} ->
              printf "%s@%d" (showBool value) level
            Just VariableData {value, reason = Implied {level, antecedent}} ->
              printf "%s@%d <- %d" (showBool value) level antecedent
        )
    | lit <- V.toList c
    ]

analyzeConflict :: Formula -> Valuation -> Int -> Clause -> (Int, Clause)
-- analyzeConflict _ v _ (simplifyClause -> clause)
--   | trace ("analyzeConflict " ++ showClauseWith v clause) False = undefined
analyzeConflict formula v currentLevel (simplifyClause -> clause) =
  case choosePivot1UIP v currentLevel clause of
    Nothing ->
      ( minimumNote
          "empty clause"
          ( V.map
              ( fromJustNote "Undecided variable in conflict clause"
                  . varLevel v
                  . atom
              )
              clause
          ),
        clause
      )
    Just antecedent ->
      analyzeConflict
        formula
        v
        currentLevel
        (resolveClauses clause (formula V.! antecedent))

type ClauseIdx = Int

unitPropagate :: Formula -> Valuation -> Either (ClauseIdx, Valuation) Valuation
unitPropagate formula initialValuation = do
  (changed, valuation) <- V.foldM unitPropagateClause (False, initialValuation) $ V.imap (,) formula
  if changed
    then unitPropagate formula valuation
    else Right valuation
  where
    unitPropagateClause :: (Bool, Valuation) -> (Int, Clause) -> Either (ClauseIdx, Valuation) (Bool, Valuation)
    unitPropagateClause (changed, v) (idx, clause) =
      case decideClause v clause of
        ClauseSAT -> Right (changed, v)
        ClauseUnresolved -> Right (changed, v)
        ClauseUNSAT -> Left (idx, v)
        ClauseUnit lit ->
          let level
                | V.length clause == 1 = 0
                | otherwise =
                    maximumNote
                      ("No antecedents for " ++ showClauseWith v clause)
                      (V.mapMaybe (varLevel v . atom) clause)
           in Right (True, learn lit (Implied {antecedent = idx, level}) v)

backtrackTo :: Int -> Valuation -> Valuation
backtrackTo level = V.map $ \case
  Just VariableData{reason}
    | reason.level >= level -> Nothing
  x -> x

checkSat :: Formula -> (Formula, Answer)
checkSat initialFormula = go initialFormula initialValuation 0
  where
    maxVar =
      join initialFormula
      & V.map atom
      & maximumNote "Empty formula"

    initialValuation = V.replicate (maxVar + 1) Nothing
    
    go :: Formula -> Valuation -> Int -> (Formula, Answer)
    go formula valuation level
      | Just lit <- chooseLit valuation formula =
          case unitPropagate formula (learn lit (Decision {level}) valuation) of
            Left (conflictIdx, propagated) ->
              let conflictClause = formula V.! conflictIdx
                  (backtrackLevel, learntClause) = analyzeConflict formula propagated level conflictClause
                  backtracked = backtrackTo backtrackLevel propagated
                  extendedFormula =
                    V.snoc
                      formula
                      learntClause
               in case unitPropagate extendedFormula backtracked of
                    Left {} -> (extendedFormula, UNSAT)
                    Right propagated2 -> go extendedFormula propagated2 backtrackLevel
            Right propagated -> go formula propagated (level + 1)
      | otherwise = (formula, SAT valuation)
