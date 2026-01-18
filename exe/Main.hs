module Main where

import Control.Monad (when)
import Data.Foldable (forM_)
import Data.IntMap qualified as IntMap
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Lilsat
import System.Environment (getArgs)
import Text.Printf (printf)

printValuation :: Formula -> Valuation -> IO ()
printValuation formula valuation =
  mapM_
    (uncurry (printValue 0))
    (sortBy (comparing (abs . fst)) $ IntMap.toList valuation)
  where
    printIndent :: Int -> IO ()
    printIndent level = do
      putStr (replicate (2 * level) ' ')
      when (level > 0) $ putStr "⤷ "

    printValue :: Int -> Int -> Reason -> IO ()
    printValue level lit reason
      | level > 4 = do
          printIndent level
          putStrLn "..."
      | otherwise = do
          printIndent level
          let atom = abs lit
              value = if lit > 0 then "" else "¬"
          printf "%s%d" value atom
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
