module Main where

import System.IO
import Data.Graph.Inductive
import qualified SS.AI.Search as S
import SS.Romenia

fromTo :: String -> String -> S.Problem String Node Int
fromTo f t = S.Problem { S.initial  = f
                       , S.actions  = neighbors graph . findByCity
                       , S.result   = curry (findByNode . snd)
                       , S.goalTest = (==t)
                       , S.stepCost = \(s,a) -> findByEdge (findByCity s) a
                       }

main :: IO ()
main = do { a <- prompt "From: "
          ; b <- prompt "To: "
          ; putStrLn $ "[bfs] "++ a ++ " -> " ++ b ++ " = " ++ (show $ S.bfs (fromTo a b))
          ; putStrLn $ "[dfs] "++ a ++ " -> " ++ b ++ " = " ++ (show $ S.dfs (fromTo a b))
          }
  where prompt s = putStr s >> hFlush stdout >> getLine
