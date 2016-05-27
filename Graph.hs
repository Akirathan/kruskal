module Graph where
import Data.List

data Graph = Graph [Node] [Edge]

data Node = Node String

{- Edge Zacatek Konec Cena -}
data Edge = Edge Node Node Int


instance Eq Node where
    (Node a) == (Node b) = a == b

{- Potrebujeme zadefinovat operator >,
-- abychom mohli na zacatku Kruskalova
-- algoritmu setridit vsechny hrany
-- podle ceny. -}
instance Ord Edge where
    (Edge _ _ x) <= (Edge _ _ y) = x <= y


instance Eq Edge where
    (Edge node1 node2 _) == (Edge node3 node4 _) =
        node1 == node3 && node2 == node4

instance Show Edge where
    show (Edge node1 node2 price) = "("++(show node1)++"->"++(show node2)++","++(show price)++")"


instance Show Node where
    show (Node s) = s



edgeSort :: [Edge]-> [Edge]
edgeSort []     = []
edgeSort (x:xs) = (edgeSort [y|y<-xs, y<x]) ++ [x] ++ (edgeSort [y|y<-xs, y>=x])



{- HRANY A VRCHOLY -}
edges1 :: () -> [Edge]
edges1 () = [(Edge (Node "a") (Node "b") 10)] ++ [(Edge (Node "b") (Node "c") 5)] ++ [(Edge (Node "c") (Node "a") 1)]

nodes1 :: ()-> [Node]
nodes1 () = [(Node "a")] ++ [(Node "b")]

