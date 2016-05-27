module Kruskal where

import Graph
import Union_find

type Skeleton = [Edge]

data State = State {
    skeleton :: Skeleton,
    bush :: [BushNode],
    graph :: Graph,
    edgeQueue :: [Edge]
    }


--jestli jsou dva BushNody ve stejne komponente
find :: State->String->String-> Bool
find state a b = root (getBushNode a bish) == root (getBushNode b bish)
             where
               bish = bush state

