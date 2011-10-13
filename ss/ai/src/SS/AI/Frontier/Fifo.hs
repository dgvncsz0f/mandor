{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module SS.AI.Frontier.Fifo
       ( FifoF()
       , fifoEmpty
       ) where

import SS.AI.Frontier
import qualified SS.Queue.Fifo as Q

newtype FifoF a = FifoF { unFifo :: Q.Fifo a }

fifoEmpty :: FifoF a
fifoEmpty = FifoF Q.empty

fifoInsert :: a -> FifoF a -> FifoF a
fifoInsert a = FifoF . Q.insert a . unFifo

fifoSelect :: FifoF a -> (a, FifoF a)
fifoSelect q = let (a, x) = Q.minView (unFifo q)
               in (a, FifoF x)

fifoMember :: (a -> Bool) -> FifoF a -> Bool
fifoMember f = Q.memberBy f . unFifo

fifoNull :: FifoF a -> Bool
fifoNull = Q.null . unFifo

instance Frontier FifoF a where

  empty     = fifoEmpty
  insert    = fifoInsert
  select    = fifoSelect
  null      = fifoNull
  member    = fifoMember