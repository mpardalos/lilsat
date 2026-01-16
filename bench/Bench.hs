{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
  ( Benchmark,
    bench,
    bgroup,
    defaultMain,
    whnf,
  )
import Data.Text.IO qualified as TIO
import Lilsat (checkSat, readCNF)
import System.FilePath (takeFileName)

-- Define file paths
satFiles :: [FilePath]
satFiles =
  [ "tests/satlib/sat/uf20-01.cnf",
    "tests/satlib/sat/uf20-010.cnf",
    "tests/satlib/sat/uf20-0100.cnf",
    "tests/satlib/sat/uf20-0101.cnf",
    "tests/satlib/sat/uf20-01000.cnf"
  ]

unsatFiles :: [FilePath]
unsatFiles =
  [ "tests/satlib/unsat/uuf50-01000.cnf",
    "tests/satlib/unsat/uuf50-0100.cnf",
    "tests/satlib/unsat/uuf50-0101.cnf",
    "tests/satlib/unsat/uuf50-0102.cnf",
    "tests/satlib/unsat/uuf50-0103.cnf"
  ]

-- Create benchmarks from a list of file paths
makeBenchmark :: FilePath -> IO Benchmark
makeBenchmark path = do
  content <- TIO.readFile path
  let formula = readCNF content
  return (bench (takeFileName path) $ whnf checkSat formula)

main :: IO ()
main = do
  satBenchmarks <- mapM makeBenchmark satFiles
  unsatBenchmarks <- mapM makeBenchmark unsatFiles
  defaultMain
    [ bgroup "SAT" satBenchmarks,
      bgroup "UNSAT" unsatBenchmarks
    ]
