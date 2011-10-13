{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module SS.AI.Frontier
       ( Frontier(..)
       ) where

class Frontier f a where

  empty     :: f a
  singleton :: a -> f a
  insert    :: a -> f a -> f a
  select    :: f a -> (a, f a)
  null      :: f a -> Bool
  member    :: (a -> Bool) -> f a -> Bool

  singleton a = insert a empty

