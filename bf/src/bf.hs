module Main where

import qualified Data.IntMap as M
import Data.Char
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import System.Environment
import System.IO

type IOFuncs       = (Value -> IO (), IO Char)
type Stdin         = String
type Value         = Int
type Address       = Int
type Memory        = M.IntMap Value
type BFRuntime a   = StateT (Memory, Address) IO a
data Instruction = MoveLeft
                 | MoveRight
                 | Increment
                 | Decrement
                 | Output
                 | Input
                 | While [Instruction]

putAddress :: Address -> BFRuntime ()
putAddress a = getMemory >>= \m -> put (m,a)

putMemory :: Memory -> BFRuntime ()
putMemory m = getAddress >>= \a -> put (m,a)

getValue :: BFRuntime (Maybe Value)
getValue = get >>= \(m,a) -> return (M.lookup a m)

putValue :: Value -> BFRuntime ()
putValue v = get >>= \(m,a) -> putMemory (M.insert a v m)

getAddress :: BFRuntime Address
getAddress = get >>= return . snd

getMemory :: BFRuntime Memory
getMemory = get >>= return . fst

maybeStack :: a -> [[a]] -> ([[a]] -> [a]) -> [a]
maybeStack a [] f       = a : f []
maybeStack a (xs:xss) f = f ((a:xs):xss)

compile :: String -> [[Instruction]] -> [Instruction]
compile [] []        = []
compile [] _         = error "syntax error"
compile ('>':xs) acc = maybeStack MoveRight acc (compile xs)
compile ('<':xs) acc = maybeStack MoveLeft acc (compile xs)
compile ('+':xs) acc = maybeStack Increment acc (compile xs)
compile ('-':xs) acc = maybeStack Decrement acc (compile xs)
compile ('.':xs) acc = maybeStack Output acc (compile xs)
compile (',':xs) acc = maybeStack Input acc (compile xs)
compile ('[':xs) acc = compile xs ([]:acc)
compile (']':xs) acc = maybeStack (While (reverse $ head acc)) (drop 1 acc) (compile xs)
compile (_:xs) acc   = compile xs acc

eval :: IOFuncs -> [Instruction] -> BFRuntime ()
eval iof = mapM_ (evalStep iof)
  where evalStep _ MoveLeft         = fmap pred getAddress >>= putAddress
        evalStep _ MoveRight        = fmap succ getAddress >>= putAddress
        evalStep _ Increment        = fmap (maybe 1 succ) getValue >>= putValue
        evalStep _ Decrement        = fmap (maybe (-1) pred) getValue >>= putValue
        evalStep iof Input          = lift (snd iof) >>= putValue . ord
        evalStep iof Output         = getValue >>= maybe (return ()) (lift . (fst iof))
        evalStep iof i@(While doI)  = do { value <- fmap (maybe 0 id) getValue
                                         ; when (value /= 0) (eval iof doI >> evalStep iof i)
                                         }

evalScript :: String -> IO ()
evalScript prg = evalStateT (eval iof (compile prg [])) (M.empty,0)
  where iof = (putChar.chr, getChar)

main :: IO ()
main = do { prg <- fmap head getArgs
          ; withFile prg ReadMode (\h -> hGetContents h >>= evalScript)
          }
