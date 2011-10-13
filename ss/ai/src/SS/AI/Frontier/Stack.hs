{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SS.AI.Frontier.Stack
       ( Stack()
       , empty
       ) where

import Prelude hiding (null)
import qualified SS.AI.Frontier as F

newtype Stack a = Stack { unPack :: [a] }

empty :: Stack a
empty = Stack []

singleton :: a -> Stack a
singleton = Stack . (:[])

insert :: a -> Stack a -> Stack a
insert a = Stack . (a :) . unPack

select :: Stack a -> (a, Stack a)
select q = let l = unPack q
           in (head l, Stack $ drop 1 l)

null :: Stack a -> Bool
null = (\l -> length l == 0) . unPack

memberBy :: (a -> Bool) -> Stack a -> Bool
memberBy f q = foldr (\a b -> f a || b) False (unPack q)

instance F.Frontier Stack a where
  
  empty  = empty
  insert = insert
  select = select
  null   = null
  member = memberBy