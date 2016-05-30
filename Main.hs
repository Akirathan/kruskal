module Main where
    
import Data.List --funkce isSubsequenceOf
import System.IO
import Graph
import Kruskal

main = do
    text <- readFile "input.txt"
    let edgez = map fromLine (lines text)
    let nodez = nodes edgez 
    let graf = (Graph nodez edgez)
    putStrLn $ show $ kruskal graf
    


--dostane celou radku
fromLine :: String-> Edge
fromLine str =
    let
        (u:v:w:rest) = words str
    in
        (Edge (read u::Node) (read v::Node) (read w::Float))


--vytahne vrcholy ze seznamu hran
nodes :: [Edge]-> [Node]
nodes edgeArray = nodes_acc [] edgeArray


nodes_acc :: [Node]->[Edge]-> [Node]
nodes_acc acc [] = acc
nodes_acc acc ((Edge u v _):xs) | notU && notV = nodes_acc (acc ++ [u,v]) xs
                                | notU         = nodes_acc (acc ++ [u]) xs
                                | notV         = nodes_acc (acc ++ [v]) xs
                                | otherwise    = nodes_acc acc xs
                                where
                                    notU = not $ isSubsequenceOf [u] acc
                                    notV = not $ isSubsequenceOf [v] acc