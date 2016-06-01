module Kruskal where

import Graph
import System.IO
import qualified Data.Set as Set


-- Skeleton musi mit take informaci o tom, jake hrany
-- jsme do nej pridali.
data Skeleton = Skeleton {
    components :: Components,
    edges :: [Edge]
    }


-- reprezentace komponenty pomoci Setu vrcholu. Set umoznuje logaritmickou
-- slozitost pri praci nad timto typem.
type Component = Set.Set Node


-- Komponenta ma svoje ID a obsah. Ve finale komponenty vypadaji napr.
-- takto: [(1,[A]),(2,[B,C]),(3,[D])], kde A je zkratka za (Node "A").
-- Pozdeji s komponentami manipulujeme pomoci ID - viz unionComponents, 
-- deleteComponent. Z praktickych duvodu je tento typ implementovan 
-- pomoci seznamu.
type Components = [(Int, Component)] 


-- vypis kostry je definovan tak, ze se vypise seznam vsech hran, ktere tato
-- kostra obsahuje.
instance Show Skeleton where
    show skeleton = show $ edges $ skeleton


{-==================================================================-}
{-                            FIND                                  -}
{-==================================================================-}


-- V dane kostre zjisti, jestli jsou dane vrcholy ve stejne komponente.
-- True vraci v pripade, ze jsou ve stejne komponente.
find :: Node->Node->Skeleton-> Bool
find u v skeleton | (findInComponents comps u) == (findInComponents comps v) = True --jsou ve stejne komponente
                  | otherwise                                                = False
                  where
                      comps = components skeleton


-- prohleda seznam vsech komponent a vrati ID te komponenty, ve ktere
-- se vyskytuje zadany vrchol.
findInComponents :: Components->Node-> Int
findInComponents (x:xs) node
    | findInComponent (snd x) node = fst x --vrchol je v x, vracime index x
    | otherwise                    = findInComponents xs node


-- V dane komponente hleda dany vrchol.
findInComponent :: Component->Node-> Bool
findInComponent comp node = Set.member node comp


{-==================================================================-}
{-                           UNION                                  -}
{-==================================================================-}

-- Prida danou hranu do dane kostry. Coz znamena, ze spoji komponenty
-- obou koncovych vrcholu. Predpoklada se, ze koncove vrcholy jsou v 
-- ruznych komponentach. Implementovano tak, ze vytvori novou kostru 
-- ze stare.
union :: Edge->Skeleton-> Skeleton
union (Edge u v w) skeleton = 
    let
        comps = components skeleton
        oldEdges = edges skeleton --uloz hrany
        i = findInComponents comps u --index komponenty, ve ktere je vrchol u
        j = findInComponents comps v --index komponenty, ve ktere je vrchol v, predpoklada se, ze i!=j
    in
        Skeleton {
            components = unionComponents comps i j, --spoj komponenty i a j do sebe
            edges = oldEdges ++ [(Edge u v w)] --pridej nove vzniklou hranu mezi vrcholy u, v
        }


-- podle indexu komponent zapoj jednu komponentu do druhe. Na pevno
-- zapojujeme druhou zadanou komponentu do prvni, protoze na poradi
-- nezalezi. V ramci zachovani jednoznacnosti ID komponent bude mit
-- komponenta vznikla spojenim dvou zadanych komponent ID prvni 
-- zadane komponenty.
unionComponents :: Components->Int->Int-> Components
unionComponents comps i j =
    let
        compi  = getComponent comps i
        compj  = getComponent comps j
        newComp  = joinComponents compi compj --spoj komponenty do sebe
        tmpComps = deleteComponent comps j --smaz j-tou komponentu ze seznamu komponent
        newComps = deleteComponent tmpComps i --smaz i-tou komponentu ze seznamu komponent
    in
        addComponent newComps (Just i) newComp  --do seznamu, ze ktereho jsme smazali i-tou a j-tou komponentu pridame newComp


-- smaze i-tou komponentu, vraci seznam komponent bez teto komponenty.
deleteComponent :: Components->Int-> Components
deleteComponent comps index = deleteComponent_acc comps index []

-- mazani komponent pomoci akumulatoru.
deleteComponent_acc :: Components->Int->Components-> Components
deleteComponent_acc (x:xs) index acc
    | fst x == index = acc ++ xs  --tuhle komponentu chci smazat - vrat akumulator a zbytek seznamu
    | otherwise      = deleteComponent_acc xs index (acc ++ [x]) 


-- prida komponentu do seznamu komponent. ID teto komponenty zvoli
-- podle druheho argumentu - pokud je to Nothing, tak 
-- ID = (velikost seznamu komponent) + 1
-- a pokud je to (Just index) tak ID = index.
addComponent :: Components->Maybe Int->Component-> Components
addComponent comps Nothing comp      = comps ++ [((length comps) + 1,comp)]
addComponent comps (Just index) comp = comps ++ [(index,comp)] 


-- zapoji druhou komponentu do prvni.
joinComponents :: Component->Component-> Component
joinComponents comp1 comp2 = Set.union comp1 comp2


-- ze zadaneho seznamu vsech komponent vraci komponentu se 
-- zadanym ID.
getComponent :: Components->Int-> Component
getComponent (x:xs) index 
    | (fst x) == index = snd x
    | otherwise        = getComponent xs index


-- zkusi pridat zadanou hranu do zadane kostry, vraci novou kostru.
-- pokud jsou koncove vrcholy hrany ve stejne komponente, tak nic 
-- neprida, pokud ne, tak tuto hranu do kostry prida.
addEdge :: Edge->Skeleton-> Skeleton
addEdge (Edge u v w) skeleton | find u v skeleton = skeleton                     --u a v jsou ve stejne komponente
                              | otherwise         = union (Edge u v w) skeleton  --nejsou ve stejne komponente


-- ukoncujici podminka pro kruskala, ze zadane kostry spocita 
-- m == n-1. Pokud ano, tak uz nema smysl zadnou hranu do kostry
-- pridavat.
enoughEdges :: Skeleton-> Bool
enoughEdges skeleton = 
    let 
        n = nodeCount skeleton --pocet vrcholu
        m = length (edges skeleton) --pocet hran aktualni kostry
    in
        if m == (n-1) 
        then True
        else False


-- spocita, kolik ma dana kostra vrcholu. Implementovano tak, ze
-- pomoci akumulatoru secte delky vsech komponent.
nodeCount :: Skeleton-> Int
nodeCount skeleton = nodeCount_acc (components skeleton) 0


nodeCount_acc :: Components->Int-> Int
nodeCount_acc [] acc     = acc
nodeCount_acc (x:xs) acc =
    let
        nodes = snd x --komponenta x
        i = length nodes --velikost komponenty x
    in
        nodeCount_acc xs (acc + i)


-- z grafu vytvori kostru tak, ze kazdy vrchol da do sve vlastni komponenty.
newSkeleton :: Graph-> Skeleton
newSkeleton g = newSkeleton_acc g [] 


newSkeleton_acc :: Graph->Components-> Skeleton
newSkeleton_acc (Graph [] _) acc     = Skeleton {components = acc, edges = []} --nova kostra nema jeste zadne hrany
newSkeleton_acc (Graph (x:xs) e) acc =
    let
        newAcc = addComponent acc Nothing (Set.fromList [x]) --do akumulatoru pridame novou komponentu, ktera ma v sobe jen vrchol x
    in
        newSkeleton_acc (Graph xs e) newAcc --rekurzime se na graf s odebranym vrcholem


{-==================================================================-}
{-                           KRUSKAL                                -}
{-==================================================================-}


kruskal :: Graph-> Skeleton
kruskal (Graph v e) = step edgeFront (newSkeleton (Graph v e))
                    where
                        edgeFront = edgeSort e --fronta hran serazena podle vahy


-- ze zadane fronty hran, setridene podle vahy, vykousne jednu hranu
-- a zkusi ji pridat do zadane kostry. Vraci nove vzniklou kostru.
-- tato funkce predstavuje jeden krok kruskalova algoritmu.
step :: [Edge]->Skeleton-> Skeleton
step [] skeleton     = skeleton --skoncili jsme
step (x:xs) skeleton =
    let 
        newSkel = addEdge x skeleton --z fronty vykousneme prvni hranu a zkusime ji pridat do kostry
    in
        if (enoughEdges newSkel)
        then step [] newSkel --"nasilne" ukoncime funkci
        else step xs newSkel 
