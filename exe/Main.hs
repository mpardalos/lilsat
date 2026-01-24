module Main where

import Control.Monad (when)
import Data.Foldable (forM_)
import Data.List (intercalate)
import Data.Text.IO qualified as TIO
import Data.Vector.Strict qualified as V
import Lilsat
import System.Environment (getArgs)
import Text.Printf (printf)

printValuation :: Formula -> Valuation -> IO ()
printValuation formula valuation =
  V.imapM_
    (printValue 0)
    valuation
  where
    printIndent :: Int -> IO ()
    printIndent level = do
      putStr (replicate (2 * level) ' ')
      when (level > 0) $ putStr "⤷ "

    printValue :: Int -> Int -> Maybe VariableData -> IO ()
    printValue nest lit Nothing = do
      printIndent nest
      printf "?%d" (abs lit)
    printValue nest lit (Just VariableData {value, reason})
      | nest > 2 = do
          printIndent nest
          putStrLn "..."
      | otherwise = do
          printIndent nest
          let atom = abs lit
              negation = if value then "" else "¬"
          case reason of
            Decision{} | nest == 0 -> putStrLn "----------"
            _ -> pure ()
          printf "%s%d" negation atom
          case reason of
            Decision {level} -> printf " by decision %d\n" level
            Implied {antecedent, level} -> do
              let clause = formula V.! antecedent
              printf " by #%d (%s) (at level %d)\n" antecedent (showClause clause) level
              forM_ clause $ \(Literal causeLit) ->
                when (abs causeLit /= abs lit) $
                   printValue (nest + 1) causeLit (valuation V.! abs causeLit)

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
    (_, UNSAT {}) -> putStrLn "UNSAT"
    (finalFormula, SAT valuation) -> do
      printValuation finalFormula valuation
      printf "Learnt %d clauses\n" (length finalFormula - length formula)
      V.iforM_ finalFormula $ \idx clause -> do
        when (idx >= length formula) $
          printf "%d: %s\n" idx (show clause)
