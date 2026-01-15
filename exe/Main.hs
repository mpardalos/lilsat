{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Data.Maybe (fromJust)
import Data.Function ((&))
import Data.List (isPrefixOf, isSuffixOf)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeFileName)
import Data.Int (Int8)
import Safe (readNote)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Atom = Int
newtype Literal = Literal Int8
  deriving Show
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

type Valuation = Atom -> Maybe Bool

data Answer = SAT Valuation | UNSAT | UNKNOWN

instance Show Answer where
  show (SAT _) = "SAT"
  show UNSAT = "UNSAT"
  show UNKNOWN = "UNKNOWN"

evalLiteral :: Valuation -> Literal -> Bool
evalLiteral valuation (Positive n) = fromJust $ valuation n
evalLiteral valuation (Negative n) = not . fromJust $ valuation n

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

  readClause :: [Int] -> Clause
  readClause [] = error "Empty clause"
  readClause [0] = []
  readClause (0:_) = error "Clause does not terminate after 0"
  readClause (x:xs)
    | x > 0 = Positive x : readClause xs
    | otherwise = Negative (-x) : readClause xs

checkSat :: Formula -> Answer
checkSat [] = UNKNOWN
checkSat (clause:clauses) =
  let !_ = checkClause clause
  in checkSat clauses
 where
   checkClause [] = UNKNOWN
   checkClause (lit:lits) = case lit of
     Positive _ -> checkClause lits
     Negative _ -> checkClause lits
    
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
