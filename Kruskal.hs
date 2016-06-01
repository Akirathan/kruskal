module Kruskal where

import Graph
import System.IO

--Skeleton musi mit take informaci o tom, jake hrany
--jsme do nej pridali.
data Skeleton = Skeleton {
    components :: Components,
    edges :: [Edge]
    }

--reprezentace komponenty pomoci seznamu vrcholu. Pro lepsi casovou
--slozitost by bylo lepsi implementovat je pomoci Set.
type Component = [Node]

--Komponenta ma svoje ID a obsah. Ve finale komponenty vypadaji napr.
--takto: [(1,[A]),(2,[B,C]),(3,[D])]. Pozdeji s komponentami manipulujeme
--pomoci ID - viz unionComponents, deleteComponent.
type Components = [(Int, Component)] 

instance Show Skeleton where
    show skeleton = show $ edges $ skeleton


{-==================================================================-}
{-                            FIND                                  -}
{-==================================================================-}

--vrati cislo komponenty, ve ktere je dany vrchol
findInComponents :: Components->Node-> Int
findInComponents (x:xs) node
    | findInComponent (snd x) node = fst x --vrchol je v x, vracime index x
    | otherwise                    = findInComponents xs node


findInComponent :: Component->Node-> Bool
findInComponent [] _ = False --prosli jsme celou komponentu a nic jsme nenasli
findInComponent (x:xs) node
    | x == node = True
    | otherwise = findInComponent xs node


--zjisti, jestli jsou dane vrcholy ve stejne komponente
find :: Node->Node->Skeleton-> Bool
find u v skeleton | (findInComponents comps u) == (findInComponents comps v) = True --jsou ve stejne komponente
                  | otherwise                                                = False
                  where
                      comps = components skeleton

{-==================================================================-}
{-                           UNION                                  -}
{-==================================================================-}

--vyhleda indexy obou vrcholu
--vraci nove vytvorenou kostru
union :: Edge->Skeleton-> Skeleton
union (Edge u v w) skeleton = 
    let
        comps = components skeleton
        oldEdges = edges skeleton
        i = findInComponents comps u --index komponenty, ve ktere je vrchol u
        j = findInComponents comps v --index komponenty, ve ktere je vrchol v, predpoklada se, ze i!=j
    in
        Skeleton {
            components = unionComponents comps i j, --spoj komponenty i a j do sebe
            edges = oldEdges ++ [(Edge u v w)]
            --ostatni parametry noveho Skeletonu
        }


--podle indexu komponent zapoj jednu komponentu do druhe
--je jedno, kterou komponentu zapojujeme do ktere
unionComponents :: Components->Int->Int-> Components
unionComponents comps i j =
    let
        compi  = getComponent comps i
        compj  = getComponent comps j
        newComp  = joinComponents compi compj
        --index = (length comps) + 1 --index takhle neni jednoznacny
        tmpComps = deleteComponent comps j --smaz j-tou komponentu ze seznamu komponent
        newComps = deleteComponent tmpComps i --smaz i-tou komponentu
    in
        addComponent newComps (Just i) newComp  --do seznamu, ze ktereho jsme smazali i-tou a j-tou komponentu pridame newComp


--smaze i-tou komponentu, vraci seznam komponent bez teto komponenty
deleteComponent :: Components->Int-> Components
deleteComponent comps index = deleteComponent_acc comps index []

--s akumulatorem
deleteComponent_acc :: Components->Int->Components-> Components
deleteComponent_acc (x:xs) index acc
    | fst x == index = acc ++ xs  --tuhle komponentu chci smazat
    | otherwise      = deleteComponent_acc xs index (acc ++ [x])


--prida komponentu do seznamu komponent.
--index nove komponenty bude o jedna vetsi, nez je 
--index posledni komponenty v Components,
--nebo tento index posleme jako argument
addComponent :: Components->Maybe Int->Component-> Components
addComponent comps Nothing comp      = comps ++ [((length comps) + 1,comp)]
addComponent comps (Just index) comp = comps ++ [(index,comp)] 


--zapoji druhou do prvni.
joinComponents :: Component->Component-> Component
joinComponents comp1 comp2 = comp1 ++ comp2


--vrati i-tou komponentu
getComponent :: Components->Int-> Component
getComponent (x:xs) index 
    | (fst x) == index = snd x
    | otherwise        = getComponent xs index



--zkusi pridat hranu do kostry, vraci novou kostru
addEdge :: Edge->Skeleton-> Skeleton
addEdge (Edge u v w) skeleton | find u v skeleton = skeleton                     --u a v jsou ve stejne komponente
                              | otherwise         = union (Edge u v w) skeleton  --nejsou ve stejne komponente

--ukoncujici podminka pro kruskala, spocita m == n-1
enoughEdges :: Skeleton-> Bool
enoughEdges skeleton = 
    let 
        n = nodeCount skeleton --pocet vrcholu
        m = length (edges skeleton) --pocet hran aktualni kostry
    in
        if m == (n-1) 
        then True
        else False

--spocita, kolik ma dana kostra vrcholu.
--secte delky vsech komponent
nodeCount :: Skeleton-> Int
nodeCount skeleton = nodeCount_acc (components skeleton) 0

nodeCount_acc :: Components->Int-> Int
nodeCount_acc [] acc     = acc
nodeCount_acc (x:xs) acc =
    let
        nodes = snd x
        i = length nodes
    in
        nodeCount_acc xs (acc + i)

--existsCycle :: Skeleton-> Bool

--z grafu vytvori kostru tak, ze kazdy vrchol da do sve vlastni komponenty.
newSkeleton :: Graph-> Skeleton
newSkeleton g = newSkeleton_acc g [] 

--s akumulatorem
newSkeleton_acc :: Graph->Components-> Skeleton
newSkeleton_acc (Graph [] _) acc     = Skeleton {components = acc, edges = []}
newSkeleton_acc (Graph (x:xs) e) acc =
    let
        newAcc = addComponent acc Nothing [x] --do akumulatoru pridame novou komponentu, ktera ma v sobe jen vrchol x
    in
        newSkeleton_acc (Graph xs e) newAcc --rekurzime se na graf s odebranym vrcholem


{-==================================================================-}
{-                           KRUSKAL                                -}
{-==================================================================-}

kruskal :: Graph-> Skeleton
kruskal (Graph v e) = func1 edgeFront (newSkeleton (Graph v e))
                    where
                        edgeFront = edgeSort e --fronta hran serazena podle vahy

--vykousne prvni hranu z fronty a zkusi ji pridat do kostry
func1 :: [Edge]->Skeleton-> Skeleton
func1 [] skeleton     = skeleton --skoncili jsme
func1 (x:xs) skeleton =
    let 
        newSkel = addEdge x skeleton
    in
        --writeSkeleton newSkel --debug
        if (enoughEdges newSkel)
        then func1 [] newSkel --zbytek hranove fronty jsme odstrihli, protoze ho uz nebudeme potrebovat
        else func1 xs newSkel


--debug
writeSkeleton :: Skeleton -> IO ()
writeSkeleton skeleton = putStrLn $ show $ skeleton

comps1 :: ()-> Components
comps1 () = [(1,[(Node "A")])] ++ [(2,[(Node "B"), (Node "C")])]

comps2 :: ()-> Components
comps2 () = [(3,[(Node "c")])] ++ [(4,[(Node "d")])] ++ [(5,[(Node "a"), (Node "b")])]

edges3 :: () -> [Edge]
edges3 () = [(Edge (Node "a") (Node "b") (1.0))]

skeleton1 :: ()-> Skeleton
skeleton1 () = Skeleton {components = (comps2 ()), edges = (edges3 ())}

component1 ::()-> Component
component1 () = [(Node "D")]

