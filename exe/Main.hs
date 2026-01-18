module Main where

import Data.Foldable (forM_)
import Data.IntMap qualified as IntMap
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Lilsat
import System.Environment (getArgs)
import Text.Printf (printf)

showClause :: Clause -> String
showClause =
  intercalate " ∨ "
    . map show
    . V.toList

printValuation :: Formula -> Valuation -> IO ()
printValuation formula valuation =
  forM_ (sortBy (comparing (abs . fst)) $ IntMap.toList valuation) $ \(lit, reason) -> do
    let atom = abs lit
        value = if lit > 0 then "✅" else "❌"
    printf "%s %d" value atom
    case reason of
      Decision -> printf " by decision\n"
      Consequence n -> printf " by #%-3d (%s)\n" n (showClause (formula V.! n))

main :: IO ()
main = do
  [path] <- getArgs
  content <- TIO.readFile path
  let formula = readCNF content
  case checkSat formula of
    UNSAT {} -> putStrLn "UNSAT"
    SAT valuation -> printValuation formula valuation
