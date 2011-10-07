module Main where

import qualified Data.IntMap as M
import Data.Maybe
import Data.Char
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import System.Environment
import System.IO

type IOFuncs m     = (Value -> m (), m Char)
type Stdin         = String
type Value         = Int
type Address       = Int
type Memory        = M.IntMap Value
type BFEnviron m a = StateT (Memory, Address) m a
data Instruction = MoveLeft
                 | MoveRight
                 | Increment
                 | Decrement
                 | Output
                 | Input
                 | While [Instruction]

putAddress :: (Monad m) => Address -> BFEnviron m ()
putAddress a = getMemory >>= \m -> put (m,a)

putMemory :: (Monad m) => Memory -> BFEnviron m ()
putMemory m = getAddress >>= \a -> put (m,a)

getValue :: (Monad m) => BFEnviron m (Maybe Value)
getValue = get >>= \(m,a) -> return (M.lookup a m)

putValue :: (Monad m) => Value -> BFEnviron m ()
putValue v = get >>= \(m,a) -> putMemory (M.insert a v m)

getAddress :: (Monad m) => BFEnviron m Address
getAddress = get >>= return . snd

getMemory :: (Monad m) => BFEnviron m Memory
getMemory = get >>= return . fst

maybePush :: a -> [[a]] -> ([[a]] -> [a]) -> [a]
maybePush a [] f       = a : f []
maybePush a (xs:xss) f = f ((a:xs):xss)

compile :: String -> [[Instruction]] -> [Instruction]
compile [] []        = []
compile [] _         = error "compile error"
compile ('>':xs) acc = maybePush MoveRight acc (compile xs)
compile ('<':xs) acc = maybePush MoveLeft acc (compile xs)
compile ('+':xs) acc = maybePush Increment acc (compile xs)
compile ('-':xs) acc = maybePush Decrement acc (compile xs)
compile ('.':xs) acc = maybePush Output acc (compile xs)
compile (',':xs) acc = maybePush Input acc (compile xs)
compile ('[':xs) acc = compile xs ([]:acc)
compile (']':xs) acc = maybePush (While (reverse $ head acc)) (drop 1 acc) (compile xs)
compile (_:xs) acc   = compile xs acc

eval :: (Monad m,Functor m) => IOFuncs m -> [Instruction] -> BFEnviron m ()
eval iof = mapM_ (evalI iof)

evalI :: (Monad m,Functor m) => IOFuncs m -> Instruction -> BFEnviron m ()
evalI _ MoveLeft         = fmap pred getAddress >>= putAddress
evalI _ MoveRight        = fmap succ getAddress >>= putAddress
evalI _ Increment        = fmap (maybe 1 succ) getValue >>= putValue
evalI _ Decrement        = fmap (maybe (-1) pred) getValue >>= putValue
evalI iof Input          = lift (snd iof) >>= putValue . ord
evalI iof Output         = getValue >>= maybe (return ()) (lift . (fst iof))
evalI iof i@(While doI)  = do { value <- fmap (maybe 0 id) getValue
                              ; when (value /= 0) (eval iof doI >> evalI iof i)
                              }

evalString :: (Monad m,Functor m) => IOFuncs m -> String -> m ()
evalString iof prg = evalStateT (eval iof (compile prg [])) (M.empty,0)

main :: IO ()
main = do { prg <- fmap head getArgs
          ; withFile prg ReadMode (\h -> hGetContents h >>= evalString (putChar.chr, getChar))
          }
