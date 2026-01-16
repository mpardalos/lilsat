{-# LANGUAGE LambdaCase #-}
module Lilsat.SolverSpec (spec) where

import Test.Hspec
import Lilsat
import Data.List (isSuffixOf)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeFileName)
import qualified Data.Text.IO as TIO

testDir :: FilePath
testDir = "tests"

spec :: Spec
spec = parallel $ do
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
        -- & take 100
  return $ map (dir </>) cnfFiles

makeTest :: (Answer -> Bool) -> FilePath -> Spec
makeTest predicate filePath =
  it (takeFileName filePath) $ do
    content <- TIO.readFile filePath
    let formula = readCNF content
    formula `shouldSatisfy` (not . null)
    let answer = checkSat formula
    answer `shouldSatisfy` predicate
    case answer of
      SAT valuation -> evalFormula valuation formula `shouldBe` True
      _ -> pure ()
