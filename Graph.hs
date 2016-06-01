module Graph where
import Data.List

data Graph = Graph [Node] [Edge] deriving (Show)

data Node = Node String

instance Eq Node where
    (Node a) == (Node b) = a == b


instance Read Node where
    readsPrec _ str = [((Node str),[])]


{- Edge Zacatek Konec Vaha -}
data Edge = Edge Node Node Float

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
    show (Edge node1 node2 weight) = "("++(show node1)++"->"++(show node2)++","++(show weight)++")"


instance Show Node where
    show (Node s) = s


edgeSort :: [Edge]-> [Edge]
edgeSort []     = []
edgeSort (x:xs) = (edgeSort [y|y<-xs, y<x]) ++ [x] ++ (edgeSort [y|y<-xs, y>=x])



-- === IO === ---





{-  HRANY A VRCHOLY -}



edges1 :: () -> [Edge]
edges1 () = [(Edge (Node "a") (Node "b") 2)] ++ [(Edge (Node "b") (Node "c") 3)] ++ [(Edge (Node "c") (Node "a") 1)]

nodes1 :: ()-> [Node]
nodes1 () = [(Node "a")] ++ [(Node "b")] ++ [(Node "c")]

--trojuhelnik
graph1 ::()-> Graph
graph1 () = Graph (nodes1 ()) (edges1 ())

node1 :: ()-> Node
node1 () = (Node "x")


graph2 ::()-> Graph
graph2 () = Graph (nodes2 ()) (edges2 ())

nodes2 :: ()-> [Node]
nodes2 () = [(Node "a")] ++ [(Node "b")] ++ [(Node "c")] ++ [(Node "d")] ++ [(Node "e")]

edges2 :: () -> [Edge]
edges2 () = [(Edge (Node "a") (Node "b") (2.5))] ++ [(Edge (Node "a") (Node "e") 2)] ++ [(Edge (Node "a") (Node "d") 10)] ++
    [(Edge (Node "b") (Node "c") 3)] ++ [(Edge (Node "b") (Node "d") 1)] ++ [(Edge (Node "c") (Node "d") 8)] ++
    [(Edge (Node "c") (Node "e") 4)] ++ [(Edge (Node "d") (Node "e") 5)]
