module Main where

import Control.Monad (when)
import Data.Foldable (forM_)
import Data.IntMap qualified as IntMap
import Data.List (intercalate, sortBy)
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Lilsat
import System.Environment (getArgs)
import Text.Printf (printf)
import Data.Ord (comparing)

printValuation :: Formula -> Valuation -> IO ()
printValuation formula valuation =
  mapM_
    (uncurry (printValue 0))
    (sortBy (comparing (reason . snd)) $ IntMap.toList valuation)
  where
    printIndent :: Int -> IO ()
    printIndent level = do
      putStr (replicate (2 * level) ' ')
      when (level > 0) $ putStr "⤷ "

    printValue :: Int -> Int -> VariableData -> IO ()
    printValue nest lit VariableData {value, reason}
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
                  let valuationLit = if IntMap.member causeLit valuation then causeLit else (-causeLit)
                   in printValue (nest + 1) valuationLit (valuation IntMap.! valuationLit)

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
