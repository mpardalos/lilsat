{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lilsat
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  [path] <- getArgs
  content <- TIO.readFile path
  let formula = readCNF content
  print $ checkSat formula
