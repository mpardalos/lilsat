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

import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Safe (headNote, readNote)

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

type Valuation = Set Literal

data Answer
  = SAT Valuation
  | UNSAT
  deriving (Show)

isSAT :: Answer -> Bool
isSAT (SAT _) = True
isSAT _ = False

evalLiteral :: Valuation -> Literal -> Maybe Bool
evalLiteral valuation lit
  | Set.member lit valuation = Just True
  | Set.member (negateLit lit) valuation = Just False
  | otherwise = Nothing -- error ("Not in valuation: " ++ show lit)

evalClause :: Valuation -> Clause -> Bool
evalClause valuation =
  V.foldr
    ( \lit acc -> case evalLiteral valuation lit of
        Just True -> True
        Just False -> acc
        Nothing -> acc || error "Missing literals"
    )
    False

-- evalClause valuation (lit : lits) = case evalLiteral valuation lit of
--   Just True -> True
--   Just False -> evalClause valuation lits
--   Nothing -> evalClause valuation lits || error "Missing literals"

evalFormula :: Valuation -> Formula -> Bool
evalFormula valuation = all (evalClause valuation)

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

simplify :: Literal -> Formula -> Formula
simplify simpLit = V.mapMaybe simplifyClause
  where
    simplifyClause :: Clause -> Maybe Clause
    simplifyClause =
      V.foldr
        ( \lit acc ->
            if
              | lit == simpLit ->
                  Nothing -- This clause is solved, delete it
              | lit == negateLit simpLit ->
                  acc -- This literal is impossible, drop it
              | otherwise ->
                  V.cons lit <$> acc -- Not this lit, continue
        )
        (Just [])

-- | Is this formula true for all assignments
isTriviallyValid :: Formula -> Bool
isTriviallyValid = null -- If no clauses, then trivially true

-- | Is this formula false for all assignments
isTriviallyUnsat :: Formula -> Bool
isTriviallyUnsat = any null -- The empty clause is unsat

chooseLit :: Formula -> Literal
chooseLit =
  headNote "Cannot choose lit from formula with empty clause"
    . V.toList
    . headNote "Cannot choose lit from empty formula"
    . V.toList

getUnitClauses :: Formula -> [Literal]
getUnitClauses =
  mapMaybe
    ( \case
        [lit] -> Just lit
        _ -> Nothing
    )
    . V.toList

simplifyIterative :: Valuation -> Formula -> (Valuation, Formula)
simplifyIterative valuation formula =
  let unitClauses = getUnitClauses formula
   in if null unitClauses
        then (valuation, formula)
        else
          simplifyIterative
            (foldr Set.insert valuation unitClauses)
            (foldr simplify formula unitClauses)

checkSat :: Formula -> Answer
checkSat = checkSatWith Set.empty
  where
    checkSatWith :: Valuation -> Formula -> Answer
    checkSatWith initialValuation initialFormula =
      let (extendedValuation, simplifiedFormula) =
            simplifyIterative initialValuation initialFormula
       in if
            | isTriviallyUnsat simplifiedFormula ->
                -- trace ("Conflict: " ++ show extendedValuation)
                UNSAT -- (error "TODO: Learned clause")
            | isTriviallyValid simplifiedFormula ->
                -- trace ("SAT!" ++ show extendedValuation)
                SAT extendedValuation
            | otherwise -> do
                let lit = chooseLit simplifiedFormula
                case checkSatWith
                  (Set.insert lit extendedValuation)
                  (simplify lit simplifiedFormula) of
                  UNSAT{} ->
                    checkSatWith
                      (Set.insert (negateLit lit) extendedValuation)
                      (simplify (negateLit lit) simplifiedFormula)
                  answer@SAT {} -> answer
