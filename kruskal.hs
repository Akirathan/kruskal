module Kruskal where
import Data.List

data Cislo = Cislo Int deriving (Show)


data Graph t = Graph [Node t] [Edge t]

data Node t = Node t

{- Edge Zacatek Konec Cena -}
data Edge t = Edge (Node t) (Node t) Int


instance (Eq t) => Eq (Node t) where
    (Node a) == (Node b) = a == b

{- Potrebujeme zadefinovat operator >,
-- abychom mohli na zacatku Kruskalova
-- algoritmu setridit vsechny hrany
-- podle ceny. -}
instance (Eq t) => Ord (Edge t) where
    (Edge _ _ x) <= (Edge _ _ y) = x <= y


instance (Eq t) => Eq (Edge t) where
    (Edge node1 node2 _) == (Edge node3 node4 _) =
        node1 == node3 && node2 == node4



edgeSort :: (Eq t) => [Edge t]-> [Edge t]
edgeSort []     = []
edgeSort (x:xs) = (edgeSort [y|y<-xs, y<x]) ++ [x] ++ (edgeSort [y|y<-xs, y>=x])

{- VSTUPY -}
edges1 :: Char -> [Edge String]
edges1 c = [(Edge (Node "a") (Node "b") 10)] ++ [(Edge (Node "b") (Node "c") 5)]
