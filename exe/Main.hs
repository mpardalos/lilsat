module Main where

import Control.Monad (when)
import Data.Foldable (forM_)
import Data.IntMap qualified as IntMap
import Data.List (intercalate)
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Lilsat
import System.Environment (getArgs)
import Text.Printf (printf)

printValuation :: Formula -> Valuation -> IO ()
printValuation formula valuation =
  mapM_
    (uncurry (printValue 0))
    (IntMap.toList valuation)
  where
    printIndent :: Int -> IO ()
    printIndent level = do
      putStr (replicate (2 * level) ' ')
      when (level > 0) $ putStr "⤷ "

    printValue :: Int -> Int -> VariableData -> IO ()
    printValue level lit VariableData {value, reason}
      | level > 4 = do
          printIndent level
          putStrLn "..."
      | otherwise = do
          printIndent level
          let atom = abs lit
              negation = if value then "" else "¬"
          printf "%s%d" negation atom
          case reason of
            Decision -> printf " by decision\n"
            Consequence n -> do
              let clause = formula V.! n
              printf " by #%d (%s)\n" n (showClause clause)
              forM_ clause $ \(Literal causeLit) ->
                when (causeLit /= lit) $
                  let valuationLit = if IntMap.member causeLit valuation then causeLit else (-causeLit)
                   in printValue (level + 1) valuationLit (valuation IntMap.! valuationLit)

    showClause :: Clause -> String
    showClause =
      intercalate " ∨ "
        . map show
        . V.toList

main :: IO ()
main = do
  [path] <- getArgs
  content <- TIO.readFile path
  let formula = readCNF content
  case checkSat formula of
    UNSAT {} -> putStrLn "UNSAT"
    SAT valuation -> printValuation formula valuation
