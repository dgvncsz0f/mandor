module SS.AI.Path 
       ( Path()
       , root
       , end
       , insert
       , elem
       , member
       , mapPath
       ) where

newtype Path s a = Path (s, [(s,a)])
                 deriving (Show)

end :: Path s a -> s
end (Path p) = fst p

insert :: Path s a -> a -> s -> Path s a
insert (Path (h, path)) a s = Path (s, (h,a) : path)

root :: s -> Path s a
root s = Path (s,[])

member :: Eq s => s -> Path s a -> Bool
member x (Path (s, path)) = x==s || x `elem` (map fst path)

mapPath :: (s -> s') -> Path s a -> Path s' a
mapPath f (Path (s, path)) = Path (f s, [(f a, b) | (a,b) <- path])