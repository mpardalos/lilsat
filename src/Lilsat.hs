{-# LANGUAGE LambdaCase #-}
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
import Control.Monad.ST (ST, runST)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable (STVector)
import Data.Vector.Mutable qualified as MV
import Safe (fromJustNote, headMay, readNote)
import Safe.Foldable (maximumNote, minimumNote)
import Text.Printf (printf)

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
learn lit reason = V.modify (learnST lit reason)

learnST :: Literal -> Reason -> STValuation s -> ST s ()
learnST (Literal lit) reason valuation = do
  MV.read valuation (abs lit) >>= \case
    Just {} -> error ("Double learn " ++ show lit)
    Nothing -> MV.write valuation (abs lit) (Just VariableData {value = lit > 0, reason})

varData :: Valuation -> Atom -> Maybe VariableData
varData v var
  | var < 0 = error "Negative var in varData"
  | otherwise = v V.! var

varDataST :: STValuation s -> Atom -> ST s (Maybe VariableData)
varDataST v var
  | var < 0 = error "Negative var in varData"
  | otherwise = MV.read v var

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

data ClauseDecision
  = ClauseSAT
  | ClauseUNSAT
  | ClauseUnresolved
  | ClauseUnit Literal

decideClauseST :: forall s. STValuation s -> Clause -> ST s ClauseDecision
decideClauseST v = V.foldM go ClauseUNSAT
  where
    go :: ClauseDecision -> Literal -> ST s ClauseDecision
    go decision lit = do
      value <- evalLiteralST v lit
      return $ case (decision, value) of
        (_, Just True) -> ClauseSAT
        (_, Just False) -> decision
        (ClauseSAT, _) -> ClauseSAT
        (ClauseUNSAT, Nothing) -> ClauseUnit lit
        (ClauseUnresolved, _) -> ClauseUnresolved
        (ClauseUnit _, Nothing) -> ClauseUnresolved

type STValuation s = STVector s (Maybe VariableData)

evalLiteralST :: STValuation s -> Literal -> ST s (Maybe Bool)
evalLiteralST valuation (Literal lit) = do
  atomValue <- fmap value <$> MV.read valuation (abs lit)
  if lit > 0
    then return atomValue
    else return (not <$> atomValue)

varLevelST :: STValuation s -> Atom -> ST s (Maybe Int)
varLevelST v var =
  varDataST v var <&> \case
    Just d -> Just d.reason.level
    Nothing -> Nothing

unitPropagateST :: forall s. Formula -> STValuation s -> ST s (Maybe ClauseIdx)
unitPropagateST formula v = do
  (changed, errorClause) <- V.ifoldM'
    ( \(changed, errorClause) idx clause ->
        if isJust errorClause
          then return (changed, errorClause)
          else
            decideClauseST v clause >>= \case
              ClauseSAT -> return (changed, Nothing)
              ClauseUnresolved -> return (changed, Nothing)
              ClauseUNSAT -> return (changed, Just idx)
              ClauseUnit lit -> do
                level <-
                  if V.length clause == 1
                    then return 0
                    else
                      V.foldM
                        ( \maxLevel nextLit ->
                            varLevelST v (atom nextLit) <&> \case
                              Just l -> max maxLevel l
                              Nothing -> maxLevel
                        )
                        0
                        clause
                learnST lit (Implied {antecedent = idx, level}) v
                return (True, Nothing)
    )
    (False, Nothing)
    formula
  case (changed, errorClause) of
    (_, Just idx) -> return (Just idx)
    (True, Nothing) -> unitPropagateST formula v
    (False, Nothing) -> return Nothing

unitPropagate :: Formula -> Valuation -> Either (ClauseIdx, Valuation) Valuation
unitPropagate formula initialValuation = runST $ do
  mValuation <- V.thaw initialValuation
  mConflict <- unitPropagateST formula mValuation
  finalValuation <- V.freeze mValuation
  return $ case mConflict of
    Just conflict -> Left (conflict, finalValuation)
    Nothing -> Right finalValuation

backtrackTo :: Int -> Valuation -> Valuation
backtrackTo level = V.map $ \case
  Just VariableData {reason}
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
