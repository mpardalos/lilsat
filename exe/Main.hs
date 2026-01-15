{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Test.Hspec
import Data.Maybe (fromJust)
import Data.Function ((&))
import Data.List (isPrefixOf, isSuffixOf)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeFileName)
import Data.Int (Int8)
import Safe (readNote, headNote)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)
import Data.Set (Set)
import qualified Data.Set as Set

type Atom = Int
newtype Literal = Literal Atom
  deriving (Show, Eq, Ord)
type Clause = [Literal]
type Formula = [Clause]

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
negateLit (Literal n) = Literal (- n)

type Valuation = Set Literal

data Answer = SAT Valuation | UNSAT | UNKNOWN

instance Show Answer where
  show (SAT _) = "SAT"
  show UNSAT = "UNSAT"
  show UNKNOWN = "UNKNOWN"

evalLiteral :: Valuation -> Literal -> Bool
evalLiteral valuation lit 
  | Set.member lit valuation = True
  | Set.member (negateLit lit) valuation = False
  | otherwise = error ("Not in valuation: " ++ show lit)

evalClause :: Valuation -> Clause -> Bool
evalClause valuation = any (evalLiteral valuation)

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
 where
  readClauseLine :: Text -> Clause
  readClauseLine = readClause . map (readNote "literal" . T.unpack) . T.words

  readClause :: [Int8] -> Clause
  readClause [] = error "Empty clause"
  readClause [0] = []
  readClause (0:_) = error "Clause does not terminate after 0"
  readClause (x:xs)
    | x > 0 = Positive x : readClause xs
    | otherwise = Negative (-x) : readClause xs

simplify :: Literal -> Formula -> Formula
simplify simpLit = mapMaybe simplifyClause
  where
    simplifyClause :: Clause -> Maybe Clause
    simplifyClause [] = Just []
    simplifyClause (lit:lits)
      | lit == simpLit =
        Nothing -- This clause is solved, delete it 
      | lit == negateLit simpLit =
        simplifyClause lits -- This literal is impossible, drop it and try the rest
      | otherwise =
        (lit :) <$> simplifyClause lits -- Not this lit, continue

-- | Is this formula true for all assignments
isTriviallyValid :: Formula -> Bool
isTriviallyValid = null -- If no clauses, then trivially true

-- | Is this formula false for all assignments
isTriviallyUnsat :: Formula -> Bool
isTriviallyUnsat = any null -- The empty clause is unsat

chooseLit :: Formula -> Literal
chooseLit =
  headNote "Cannot choose lit from formula with empty clause"
  . headNote "Cannot choose lit from empty formula"

checkSat :: Formula -> Answer
checkSat = checkSatWith Set.empty
  where
    checkSatWith :: Valuation -> Formula -> Answer
    checkSatWith valuation formula
      | isTriviallyUnsat formula =
        trace ("Best I managed was: " ++ show valuation) UNSAT
      | isTriviallyValid formula = SAT valuation
      | otherwise =
        let lit = chooseLit formula
        in checkSatWith (Set.insert lit valuation) (simplify lit formula)

testDir :: FilePath
testDir = "tests"

main :: IO ()
main = hspec $ do
  describe "SATLIB" $ do
    satTests <- runIO $ discoverTests (testDir </> "satlib" </> "sat")
    describe "SAT (satisfiable)" $ do
      mapM_ (makeTest (\case { SAT _ -> True; _ -> False })) satTests

    unsatTests <- runIO $ discoverTests (testDir </> "satlib" </> "unsat")
    describe "UNSAT (unsatisfiable)" $ do
      mapM_ (makeTest (\case { UNSAT -> True; _ -> False })) unsatTests

discoverTests :: FilePath -> IO [FilePath]
discoverTests dir = do
  files <- listDirectory dir
  let cnfFiles =
        filter (\f -> ".cnf" `isSuffixOf` f) files
        & take 2
  return $ map (dir </>) cnfFiles

makeTest :: (Answer -> Bool) -> FilePath -> Spec
makeTest predicate filePath =
  it (takeFileName filePath) $ do
    content <- TIO.readFile filePath
    let formula = readCNF content
    formula `shouldSatisfy` (not . null)
    checkSat formula `shouldSatisfy` predicate
