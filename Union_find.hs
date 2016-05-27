module Union_find where

data BushNode = BushNode {
     name :: String 
   , otec :: BushNode 
   , depth :: Int
   } | Nill deriving (Show)

root :: BushNode-> BushNode
root Nill     = Nill
root bushNode = root p
              where
                p = otec bushNode



{-------------}


