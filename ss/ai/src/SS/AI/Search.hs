{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module SS.AI.Search
       ( Problem(..)
       , bfs
       , dfs
       ) where

import Control.Monad
import Control.Monad.State
import qualified Data.Set as S
import qualified SS.AI.Path as P
import qualified SS.AI.Frontier as F
import qualified SS.AI.Frontier.Fifo as Fifo
import qualified SS.AI.Frontier.Stack as Stack

data Problem s a c = Problem { initial  :: s
                             , actions  :: s -> [a]
                             , result   :: s -> a -> s
                             , goalTest :: s -> Bool
                             , stepCost :: (s,a) -> c
                             }

bfs :: (Ord s) => Problem s a c -> P.Path s a
bfs p = let frontier = F.insert (P.root (initial p)) Fifo.empty
            explored = S.empty
        in evalState (solveGraphSearch p) (frontier, explored)

dfs :: (Ord s) => Problem s a c -> P.Path s a
dfs p = let frontier = F.insert (P.root (initial p)) Stack.empty
            explored = S.empty
        in evalState (solveGraphSearch p) (frontier, explored)

solveGraphSearch :: (Ord s, F.Frontier f (P.Path s a)) => Problem s a c -> State (f (P.Path s a), S.Set s) (P.Path s a)
solveGraphSearch p = 
  do { frontier <- select
     ; case frontier
       of Nothing
            -> fail "goal not found"
          Just path
            -> let s = P.end path
               in if (goalTest p s)
                  then return path
                  else addToExplored s >> addToFrontier p path >> solveGraphSearch p
     }

getSnd :: State (a,b) b
getSnd = fmap snd get

putSnd :: b -> State (a,b) ()
putSnd b = getFst >>= \a -> put (a,b)

updateSnd :: (b -> b) -> State (a,b) ()
updateSnd f = getSnd >>= putSnd . f

getFst :: State (a,b) a
getFst = fmap fst get

putFst :: a -> State (a,b) ()
putFst a = getSnd >>= \b -> put (a,b)

updateFst :: (a -> a) -> State (a,b) ()
updateFst f = getFst >>= putFst . f

select :: (F.Frontier f (P.Path s a)) => State (f (P.Path s a), b) (Maybe (P.Path s a))
select = do { frontier <- getFst
            ; case frontier
              of (F.null -> True)
                   -> return Nothing
                 (F.select -> (path, frontier'))
                   -> putFst frontier' >> return (Just path)
            }

addToExplored :: (Ord s) => s -> State (a, S.Set s) ()
addToExplored s = updateSnd (S.insert s)

visited :: (Ord s, F.Frontier f (P.Path s a)) => s -> State (f (P.Path s a), S.Set s) Bool
visited s = do { isExplored <- fmap (S.member s) getSnd
               ; onFrontier <- fmap (F.member (P.member s)) getFst
               ; return (isExplored || onFrontier)
               }

addToFrontier :: (Ord s, F.Frontier f (P.Path s a)) => Problem s a c -> P.Path s a -> State (f (P.Path s a), S.Set s) ()
addToFrontier p path = mapM_ insertAction (actions p s)
  where s = P.end path
        
        insertAction a = let s' = result p s a
                             p' = P.insert path a s'
                         in visited s' >>= \v -> when (not v) (updateFst (F.insert p'))

