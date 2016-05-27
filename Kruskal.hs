module Kruskal where

import Graph
import Union_find

convertNode :: Node-> BushNode
convertNode (Node n) = (BushNode n Nill (-1))


