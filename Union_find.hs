module Union_find where

data BushNode = BushNode {
     name :: String 
   , otec :: BushNode 
   , depth :: Int
   } | Nill deriving (Show)


instance Eq BushNode where
    bushNode1 == bushNode2 = (name bushNode1) == (name bushNode2)


root :: BushNode-> BushNode
root Nill     = Nill
root bushNode = root p
              where
                p = otec bushNode


--podle nazvu vybere ze vsech keriku dany BushNode
getBushNode :: String->[BushNode]-> BushNode
getBushNode _ []       = Nill
getBushNode str (x:xs) | (name x) == str = x
                       | otherwise       = getBushNode str xs

{-------------}

bush1 :: ()-> [BushNode]
bush1 () = [(BushNode "b" (BushNode "a" Nill 1) (-1))] ++ [(BushNode "a" Nill 1)]
