module Graph where

data Graph = Graph [Node] [Edge] deriving (Show)

data Node = Node String

instance Eq Node where
    (Node a) == (Node b) = a == b

instance Ord Node where
    (Node a) <= (Node b) = a < b

instance Show Node where
    show (Node s) = s

instance Read Node where
    readsPrec _ str = [((Node str),[])]


-- Edge Zacatek Konec Vaha
data Edge = Edge Node Node Float

-- potrebujeme zadefinovat usporadani hran, abychom
-- je na zacatku kruskalova algoritmu mohli vsechny
-- setridit.
instance Ord Edge where
    (Edge _ _ x) <= (Edge _ _ y) = x <= y


-- hrany jsou stejne, pokud maji stejny zacatecni a 
-- koncovy vrchol.
instance Eq Edge where
    (Edge node1 node2 _) == (Edge node3 node4 _) =
        node1 == node3 && node2 == node4

instance Show Edge where
    show (Edge node1 node2 weight) = "("++(show node1)++"->"++(show node2)++","++(show weight)++")"


-- trideni hran implementovane pomoci quicksortu.
edgeSort :: [Edge]-> [Edge]
edgeSort []     = []
edgeSort (x:xs) = (edgeSort [y|y<-xs, y<x]) ++ [x] ++ (edgeSort [y|y<-xs, y>=x])
