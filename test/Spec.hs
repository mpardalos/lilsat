{-# LANGUAGE LambdaCase #-}
module Main where

import Test.Hspec
import Lilsat
import Data.List (isSuffixOf)
import System.Directory (listDirectory, createDirectoryIfMissing, removeFile, doesDirectoryExist)
import System.FilePath ((</>), takeFileName, takeBaseName, takeDirectory)
import qualified Data.Text.IO as TIO
import System.Process (callCommand)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Control.Exception (evaluate)

testDir :: FilePath
testDir = "tests"

main :: IO ()
main = do
  getSATLIBTests "flat30-60" "sat" "https://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/GCP/flat30-60.tar.gz"
  getSATLIBTests "sw100-8-lp0-c5" "sat" "https://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/SW-GCP/sw100-8-lp0-c5.tar.gz"
  getSATLIBTests "planning" "sat" "https://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/PLANNING/BlocksWorld/blocksworld.tar.gz"

  (satTests, unsatTests) <- discoverTests testDir

  hspec $ parallel $ do
    describe "SATLIB" $ do
      describe "SAT (satisfiable)" $ do
        mapM_ (makeTest (\case { SAT _ -> True; _ -> False })) satTests

      describe "UNSAT (unsatisfiable)" $ do
        mapM_ (makeTest (\case { UNSAT -> True; _ -> False })) unsatTests

getSATLIBTests :: String -> String -> String -> IO ()
getSATLIBTests suite category url = do
  let targetDir = testDir </> "satlib" </> suite </> category

  exists <- doesDirectoryExist targetDir
  if exists
    then putStrLn $ "Skipping " ++ suite ++ " (directory already exists: " ++ targetDir ++ ")"
    else do
      createDirectoryIfMissing True targetDir

      withSystemTempDirectory "satlib-download" $ \tmpDir -> do
        let tarFileName = takeFileName url
        let tarFilePath = tmpDir </> tarFileName

        -- Download the tarball
        putStrLn $ "Downloading " ++ suite ++ " from " ++ url
        callCommand $ "curl -L -o " ++ show tarFilePath ++ " " ++ show url

        -- Extract to temp directory
        putStrLn $ "Extracting " ++ tarFileName
        callCommand $ "tar -xzf " ++ show tarFilePath ++ " -C " ++ show tmpDir

        -- Move .cnf files to target directory
        putStrLn $ "Moving CNF files to " ++ targetDir
        callCommand $ "find " ++ show tmpDir ++ " -name '*.cnf' -exec mv {} " ++ show targetDir ++ " \\;"

discoverTests :: FilePath -> IO ([FilePath], [FilePath])
discoverTests baseDir = do
  allCnfFiles <- findCNFFiles baseDir
  let categorized = map categorizeTest allCnfFiles
  let satTests = [fp | (fp, "sat") <- categorized]
  let unsatTests = [fp | (fp, "unsat") <- categorized]
  return (satTests, unsatTests)
  where
    categorizeTest :: FilePath -> (FilePath, String)
    categorizeTest fp =
      let parentDir = takeFileName $ takeDirectory fp
      in if parentDir `elem` ["sat", "unsat"]
         then (fp, parentDir)
         else error $ "Test file " ++ fp ++ " is not in a 'sat' or 'unsat' directory (found in: " ++ parentDir ++ ")"

findCNFFiles :: FilePath -> IO [FilePath]
findCNFFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return []
    else do
      entries <- listDirectory dir
      let fullPaths = map (dir </>) entries
      results <- mapM processEntry fullPaths
      return $ concat results
  where
    processEntry :: FilePath -> IO [FilePath]
    processEntry path = do
      isDir <- doesDirectoryExist path
      if isDir
        then findCNFFiles path
        else if ".cnf" `isSuffixOf` path
             then return [path]
             else return []

makeTest :: (Answer -> Bool) -> FilePath -> Spec
makeTest predicate filePath =
  it (takeFileName filePath) $ do
    content <- TIO.readFile filePath
    let formula = readCNF content
    formula `shouldSatisfy` (not . null)
    mAnswer <- timeout 10_000_000 $ evaluate (checkSat formula)
    case mAnswer of
      Nothing -> pendingWith "Timed out"
      Just answer -> do
        answer `shouldSatisfy` predicate
        case answer of
          SAT valuation -> evalFormula valuation formula `shouldBe` Just True
          _ -> pure ()
