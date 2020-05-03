{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = N
  { state :: s
  , action :: Maybe a
  , parent :: Maybe (Node s a)
  , depth :: Int
  , children :: [Node s a]
  } deriving (Show, Eq)

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState (N st _ _ _ _) = st

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (N _ _ p _ _)= p

nodeDepth :: Node s a -> Int
nodeDepth (N _ _ _ d _) = d

nodeAction :: Node s a -> Maybe a
nodeAction (N _ act _ _ _)= act

nodeChildren :: Node s a -> [Node s a]
nodeChildren (N _ _ _ _ ch) = ch

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}
createStateChild :: (ProblemState s a, Eq s) => s -> Node s a -> a -> Int -> Node s a
createStateChild st father act d = genNode
    where
        genNode = (N st (Just act) (Just father) d (createKids st (successors st)))
        createKids _ [] = []
        createKids currentState (x:xs) = [(createStateChild (snd x) genNode (fst x) (d + 1))] ++ (createKids currentState xs)
createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace st = genNode
    where
        genNode = (N st Nothing Nothing 0 (createKids st (successors st)))
        createKids _ [] = []
        createKids status (x:xs) = [(createStateChild (snd x) genNode (fst x) 1)] ++ (createKids status xs)

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}
getKids :: Ord s => [Node s a] -> [s] -> [([Node s a], [Node s a])]
getKids queue viz = [(genCurrent kids, (genFront kids))] ++ ((getKids (genFront kids)) ((nodeState (head queue)) : viz))
    where
        genCurrent [] = []
        genCurrent (k:ks) = if elem (nodeState k) viz then (genCurrent ks) else k : (genCurrent ks)
        genFront ks = (tail queue) ++ (genCurrent ks)
        kids = nodeChildren $ head queue

bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs node = getKids [node] []


{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS start finish = ((getNodeSt (bfs start) (bfs finish)) , (getNodeFin (bfs start) (bfs finish)))
    where
        getNodeSt (elem1:stlist) (elem2:fnlist) = if null [a | a <- (fst elem1), b <- (snd elem2), (checkLevel a b)] 
                                                 then (getNodeSt stlist fnlist)
                                                 else (head [a | a <- (fst elem1), b <- (snd elem2), (checkLevel a b)])
            where
               checkLevel lv1 lv2 = if (nodeState lv1 == nodeState lv2) then True else False

        getNodeFin (elem1:stlist) (elem2:fnlist) = if null [b | a <- (fst elem1), b <- (snd elem2), (checkLevel a b)] 
                                                 then (getNodeFin stlist fnlist)
                                                 else (head [b | a <- (fst elem1), b <- (snd elem2), (checkLevel a b)])
            where
               checkLevel lv1 lv2 = if (nodeState lv1 == nodeState lv2) then True else False
{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPath :: Node s a -> [(Maybe a, s)]
extractPath = undefined



{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve = undefined
