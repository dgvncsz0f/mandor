module SS.Romenia where

import Data.Graph.Inductive
import Data.List as L

cities :: [LNode String]
cities = zip [1..] [ "Arad"
                   , "Zerind"
                   , "Sibiu"
                   , "Timisoara"
                   , "Oradea"
                   , "Fagaras"
                   , "RimnicuVilcea"
                   , "Lugoj"
                   , "Bucharest"
                   , "Pitesti"
                   , "Craiova"
                   , "Mehadia"
                   , "Urziceni"
                   , "Giurgiu"
                   , "Dobreta"
                   , "Vaslui"
                   , "Hirsova"
                   , "Iasi"
                   , "Eforie"
                   , "Neamt"
                   ]

routes :: [LEdge Int]
routes = concat $ [ mkUoEdge "Arad" "Sibiu" 140
                  , mkUoEdge "Arad" "Timisoara" 118
                  , mkUoEdge "Arad" "Zerind" 75
                  , mkUoEdge "Zerind" "Oradea" 71
                  , mkUoEdge "Sibiu" "Fagaras" 99
                  , mkUoEdge "Sibiu" "RimnicuVilcea" 80
                  , mkUoEdge "Timisoara" "Lugoj" 118
                  , mkUoEdge "Oradea" "Sibiu" 151
                  , mkUoEdge "Fagaras" "Bucharest" 211
                  , mkUoEdge "RimnicuVilcea" "Pitesti" 97
                  , mkUoEdge "RimnicuVilcea" "Craiova" 146
                  , mkUoEdge "Lugoj" "Mehadia" 75
                  , mkUoEdge "Pitesti" "Bucharest" 101 
                  , mkUoEdge "Pitesti" "Craiova" 138
                  , mkUoEdge "Mehadia" "Dobreta" 75
                  , mkUoEdge "Dobreta" "Craiova" 120
                  , mkUoEdge "Bucharest" "Urziceni" 85
                  , mkUoEdge "Bucharest" "Giurgiu" 90
                  , mkUoEdge "Urziceni" "Hirsova" 98
                  , mkUoEdge "Urziceni" "Vaslui" 142
                  , mkUoEdge "Vaslui" "Iasi" 92
                  , mkUoEdge "Hirsova" "Eforie" 86
                  , mkUoEdge "Iasi" "Neamt" 87
                  ]

fromJust :: String -> Maybe a -> a
fromJust msg Nothing = error msg
fromJust _ (Just a)  = a

findByCity :: String -> Node
findByCity c = fst $ fromJust ("city: "++ c) $ L.find ((==c) . snd) cities

findByNode :: Node -> String
findByNode i = snd $ fromJust ("index: "++ (show i)) $ L.find ((==i) . fst) cities

findByEdge :: Node -> Node -> Int
findByEdge a b = trd $ fromJust ("edge: "++ (show (a,b))) $ L.find (\(c,d,_) -> (c,d) == (a,b)) routes

trd :: (a,b,c) -> c
trd (_,_,c) = c

mkUoEdge :: String -> String -> c -> [(Node, Node, c)]
mkUoEdge c0 c1 cost = let ci0 = findByCity c0
                          ci1 = findByCity c1
                      in [(ci0, ci1, cost)]

graph :: Gr String Int
graph = mkGraph cities routes

