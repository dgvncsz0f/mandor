module SS.Queue.Fifo 
       ( Fifo()
       , empty
       , singleton
       , fromList
       , insert
       , minView
       , null
       , member
       , memberBy
       ) where

import Prelude hiding (null)
import Data.Maybe (fromJust)
import qualified Data.Map as M

newtype Fifo a = Fifo { unPack :: M.Map Int a }
                 deriving (Show)

empty :: Fifo a
empty = Fifo M.empty

fromList :: [a] -> Fifo a
fromList = foldl (flip insert) empty

null :: Fifo a -> Bool
null = M.null . unPack

singleton :: a -> Fifo a
singleton = Fifo . M.singleton 0

insert :: a  -> Fifo a -> Fifo a
insert a (Fifo m) 
  | M.null m  = singleton a
  | otherwise = let (k,_) = M.findMax m
                in Fifo $ M.insert (k+1) a m

minView :: Fifo a -> (a, Fifo a)
minView q = fromJust $ do { (a, m) <- M.minView (unPack q)
                          ; return (a, Fifo m)
                          }

member :: (Eq a) => a -> Fifo a -> Bool
member a q = a `elem` map snd (M.toList $ unPack q)

memberBy :: (a -> Bool) -> Fifo a -> Bool
memberBy f q = foldr (\a b -> f (snd a) || b) False (M.toList $ unPack q)