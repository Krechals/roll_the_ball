{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes as P
import ProblemState
import Data.Array as A


{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = C
  { 
    celltype :: Char
  } deriving (Show, Eq, Ord)
{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = L
 { 
    matrix :: (A.Array (Int, Int) Char)
 } deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

instance Show Level where 
    show (L board) = "\n" ++ (transform_matrix (A.elems board) (snd (snd (A.bounds board))) 0)
        where
        transform_matrix [] _ _ = "\n"
        transform_matrix arr cols current_col 
            | current_col == cols + 1 = "\n" ++ (transform_matrix arr cols 0)
            | otherwise = [(head arr)] ++ (transform_matrix (tail arr) cols (current_col + 1))

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel down_right = (L (A.array ((0, 0), down_right) (matrix_build 0 0 down_right)))
    where
        matrix_build line col corner
            | (line == (fst corner)) && (col == (snd corner)) = [((line, col), emptySpace)]
            | (col == (snd corner)) = [((line, col), emptySpace)] ++ (matrix_build (line + 1) 0 corner)
            | otherwise  = [((line, col), emptySpace)] ++ (matrix_build line (col + 1) corner)

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

checkElement :: Position -> Level -> Bool
checkElement point (L board)
            | (fst point) < 0 = False
            | (snd point) < 0 = False
            | (fst point) > (fst (snd (A.bounds board))) = False
            | (snd point) > (snd (snd (A.bounds board))) = False
            | otherwise = True

addCell :: (Char, Position) -> Level -> Level
addCell element lvl@(L board) = if (checkElement (snd element) lvl) 
                                then (L (board A.// [((snd element), (fst element))]))
                                else lvl


{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel right_bottom elements = foldr (\ element board -> (addCell element board)) (emptyLevel right_bottom) elements


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

checkEmptySpace :: Position -> Level -> Bool
checkEmptySpace point (L board)
            | (fst point) < 0 = False
            | (snd point) < 0 = False
            | (fst point) > (fst (snd (A.bounds board))) = False
            | (snd point) > (snd (snd (A.bounds board))) = False
            | (board A.! point) /= emptySpace = False
            | otherwise = True

checkStart :: Position -> Level -> Bool
checkStart point (L board)
            | (board A.! point) == startUp = False
            | (board A.! point) == startDown = False
            | (board A.! point) == startLeft = False
            | (board A.! point) == startRight = False
            | (board A.! point) == winUp = False
            | (board A.! point) == winDown = False
            | (board A.! point) == winLeft = False
            | (board A.! point) == winRight = False
            | otherwise = True

moveCell :: Position -> Directions -> Level -> Level
moveCell pos North lvl@(L board) = if (checkEmptySpace ((fst pos) - 1, (snd pos)) lvl) && (checkStart pos lvl) 
                                   then (addCell (emptySpace, pos) (addCell ((board A.! pos), ((fst pos) - 1, (snd pos))) lvl))
                                   else lvl
moveCell pos South lvl@(L board) = if (checkEmptySpace ((fst pos) + 1, (snd pos)) lvl) && (checkStart pos lvl) 
                                   then (addCell (emptySpace, pos) (addCell ((board A.! pos), ((fst pos) + 1, (snd pos))) lvl))
                                   else lvl
moveCell pos East lvl@(L board) = if (checkEmptySpace ((fst pos), (snd pos) + 1) lvl) && (checkStart pos lvl) 
                                   then (addCell (emptySpace, pos) (addCell ((board A.! pos), ((fst pos), (snd pos) + 1)) lvl))
                                   else lvl
moveCell pos West lvl@(L board) = if (checkEmptySpace ((fst pos), (snd pos) - 1) lvl) && (checkStart pos lvl) 
                                   then (addCell (emptySpace, pos) (addCell ((board A.! pos), ((fst pos), (snd pos) - 1)) lvl))
                                   else lvl        





{-
    *** TODO ***

    Verifică dacă două celule se pot conecta.
    Atenție: Această funcție se aplică de la stânga la 
    dreapta(nu este comutativă).

    ex: connection botLeft horPipe = True (╚═)
        connection horPipe botLeft = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection (C celltype1) (C celltype2) East
    | (celltype1 == horPipe) && (celltype2 == horPipe) = True
    | (celltype1 == horPipe) && (celltype2 == botRight) = True
    | (celltype1 == horPipe) && (celltype2 == topRight) = True
    | (celltype1 == horPipe) && (celltype2 == startLeft) = True
    | (celltype1 == horPipe) && (celltype2 == winLeft) = True
    | (celltype1 == topLeft) && (celltype2 == horPipe) = True
    | (celltype1 == topLeft) && (celltype2 == botRight) = True
    | (celltype1 == topLeft) && (celltype2 == topRight) = True
    | (celltype1 == topLeft) && (celltype2 == startLeft) = True
    | (celltype1 == topLeft) && (celltype2 == winLeft) = True
    | (celltype1 == botLeft) && (celltype2 == horPipe) = True
    | (celltype1 == botLeft) && (celltype2 == botRight) = True
    | (celltype1 == botLeft) && (celltype2 == topRight) = True
    | (celltype1 == botLeft) && (celltype2 == startLeft) = True
    | (celltype1 == botLeft) && (celltype2 == winLeft) = True
    | (celltype1 == startRight) && (celltype2 == horPipe) = True
    | (celltype1 == startRight) && (celltype2 == botRight) = True
    | (celltype1 == startRight) && (celltype2 == topRight) = True
    | (celltype1 == startRight) && (celltype2 == startLeft) = True
    | (celltype1 == startRight) && (celltype2 == winLeft) = True
    | (celltype1 == winRight) && (celltype2 == horPipe) = True
    | (celltype1 == winRight) && (celltype2 == botRight) = True
    | (celltype1 == winRight) && (celltype2 == topRight) = True
    | (celltype1 == winRight) && (celltype2 == startLeft) = True
    | (celltype1 == winRight) && (celltype2 == winLeft) = True
    | otherwise = False
connection (C celltype1) (C celltype2) West
    | (celltype1 == horPipe) && (celltype2 == horPipe) = True
    | (celltype1 == horPipe) && (celltype2 == topLeft) = True
    | (celltype1 == horPipe) && (celltype2 == botLeft) = True
    | (celltype1 == horPipe) && (celltype2 == startRight) = True
    | (celltype1 == horPipe) && (celltype2 == winRight) = True
    | (celltype1 == botRight) && (celltype2 == horPipe) = True
    | (celltype1 == botRight) && (celltype2 == topLeft) = True
    | (celltype1 == botRight) && (celltype2 == botLeft) = True
    | (celltype1 == botRight) && (celltype2 == startRight) = True
    | (celltype1 == botRight) && (celltype2 == winRight) = True
    | (celltype1 == topRight) && (celltype2 == winRight) = True
    | (celltype1 == topRight) && (celltype2 == horPipe) = True
    | (celltype1 == topRight) && (celltype2 == topLeft) = True
    | (celltype1 == topRight) && (celltype2 == botLeft) = True
    | (celltype1 == topRight) && (celltype2 == startRight) = True
    | (celltype1 == topRight) && (celltype2 == winRight) = True
    | (celltype1 == startLeft) && (celltype2 == horPipe) = True
    | (celltype1 == startLeft) && (celltype2 == topLeft) = True
    | (celltype1 == startLeft) && (celltype2 == botLeft) = True
    | (celltype1 == startLeft) && (celltype2 == startRight) = True
    | (celltype1 == startLeft) && (celltype2 == winRight) = True
    | (celltype1 == winLeft) && (celltype2 == horPipe) = True
    | (celltype1 == winLeft) && (celltype2 == topLeft) = True
    | (celltype1 == winLeft) && (celltype2 == botLeft) = True
    | (celltype1 == winLeft) && (celltype2 == startRight) = True
    | (celltype1 == winLeft) && (celltype2 == winRight) = True
    | otherwise = False
connection (C celltype1) (C celltype2) North
    | (celltype1 == verPipe) && (celltype2 == verPipe) = True
    | (celltype1 == verPipe) && (celltype2 == topLeft) = True
    | (celltype1 == verPipe) && (celltype2 == topRight) = True
    | (celltype1 == verPipe) && (celltype2 == startDown) = True
    | (celltype1 == verPipe) && (celltype2 == winDown) = True
    | (celltype1 == botLeft) && (celltype2 == verPipe) = True
    | (celltype1 == botLeft) && (celltype2 == topLeft) = True
    | (celltype1 == botLeft) && (celltype2 == topRight) = True
    | (celltype1 == botLeft) && (celltype2 == startDown) = True
    | (celltype1 == botLeft) && (celltype2 == winDown) = True
    | (celltype1 == botRight) && (celltype2 == verPipe) = True
    | (celltype1 == botRight) && (celltype2 == topLeft) = True
    | (celltype1 == botRight) && (celltype2 == topRight) = True
    | (celltype1 == botRight) && (celltype2 == startDown) = True
    | (celltype1 == botRight) && (celltype2 == winDown) = True
    | (celltype1 == startUp) && (celltype2 == verPipe) = True
    | (celltype1 == startUp) && (celltype2 == topLeft) = True
    | (celltype1 == startUp) && (celltype2 == topRight) = True
    | (celltype1 == startUp) && (celltype2 == startDown) = True
    | (celltype1 == startUp) && (celltype2 == winDown) = True
    | (celltype1 == winUp) && (celltype2 == verPipe) = True
    | (celltype1 == winUp) && (celltype2 == topLeft) = True
    | (celltype1 == winUp) && (celltype2 == topRight) = True
    | (celltype1 == winUp) && (celltype2 == startDown) = True
    | (celltype1 == winUp) && (celltype2 == winDown) = True
    | otherwise = False
connection (C celltype1) (C celltype2) South
    | (celltype1 == verPipe) && (celltype2 == verPipe) = True
    | (celltype1 == verPipe) && (celltype2 == botLeft) = True
    | (celltype1 == verPipe) && (celltype2 == botRight) = True
    | (celltype1 == verPipe) && (celltype2 == startUp) = True
    | (celltype1 == verPipe) && (celltype2 == winUp) = True
    | (celltype1 == topLeft) && (celltype2 == verPipe) = True
    | (celltype1 == topLeft) && (celltype2 == botLeft) = True
    | (celltype1 == topLeft) && (celltype2 == botRight) = True
    | (celltype1 == topLeft) && (celltype2 == startUp) = True
    | (celltype1 == topLeft) && (celltype2 == winUp) = True
    | (celltype1 == topRight) && (celltype2 == verPipe) = True
    | (celltype1 == topRight) && (celltype2 == botLeft) = True
    | (celltype1 == topRight) && (celltype2 == botRight) = True
    | (celltype1 == topRight) && (celltype2 == startUp) = True
    | (celltype1 == topRight) && (celltype2 == winUp) = True
    | (celltype1 == startDown) && (celltype2 == verPipe) = True
    | (celltype1 == startDown) && (celltype2 == botLeft) = True
    | (celltype1 == startDown) && (celltype2 == botRight) = True
    | (celltype1 == startDown) && (celltype2 == startUp) = True
    | (celltype1 == startDown) && (celltype2 == winUp) = True
    | (celltype1 == winDown) && (celltype2 == verPipe) = True
    | (celltype1 == winDown) && (celltype2 == botLeft) = True
    | (celltype1 == winDown) && (celltype2 == botRight) = True
    | (celltype1 == winDown) && (celltype2 == startUp) = True
    | (celltype1 == winDown) && (celltype2 == winUp) = True
    | otherwise = False
{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}
startChars :: [Char]
startChars = [startUp, startDown, startLeft, startRight]

winChars :: [Char]
winChars = [winUp, winDown, winLeft, winRight]

wonLevel :: Level -> Bool
wonLevel lvl@(L board)= (dfs (start_point 0 0 (snd (A.bounds board)) lvl) 
                             (-1, -1)
                             (finish_point 0 0 (snd (A.bounds board)) lvl) lvl)
    where
        start_point line col right_bottom l@(L board2)
            | (line == (fst right_bottom) + 1) = (-1, -1)
            | (elem (board2 A.! (line, col)) startChars) = (line, col) 
            | col == (snd right_bottom) = start_point (line + 1) 0 right_bottom l
            | otherwise = start_point line (col + 1) right_bottom l

        finish_point line col right_bottom l@(L board2)
            | (line == (fst right_bottom) + 1) = (-1, -1)
            | (elem (board2 A.! (line, col)) winChars) = (line, col) 
            | col == (snd right_bottom) = finish_point (line + 1) 0 right_bottom l
            | otherwise = finish_point line (col + 1) right_bottom l

        dfs start prev finish lv@(L board3)
            | start == (-1, -1) = False
            | finish == (-1, -1) = False
            | start == finish = True
            | (checkElement ((fst start + 1), (snd start)) lv)
               && (((fst start + 1), (snd start)) /= prev)
               && (connection (C (board A.! start)) (C (board A.! ((fst start + 1), (snd start)))) South) =  dfs ((fst start + 1), (snd start)) start finish lv
            | (checkElement ((fst start - 1), (snd start)) lv) 
               && (((fst start - 1), (snd start)) /= prev)
               && (connection (C (board A.! start)) (C (board A.! ((fst start - 1), (snd start)))) North) =  dfs ((fst start - 1), (snd start)) start finish lv
            | (checkElement ((fst start), (snd start + 1)) lv) 
               && (((fst start), (snd start) + 1) /= prev)
               && (connection (C (board A.! start)) (C (board A.! ((fst start), (snd start + 1)))) East) =  dfs ((fst start), (snd start + 1)) start finish lv
            | (checkElement ((fst start), (snd start - 1)) lv) 
               && (((fst start), (snd start) - 1) /= prev)
               && (connection (C (board A.! start)) (C (board A.! ((fst start), (snd start - 1)))) West) =  dfs ((fst start), (snd start - 1)) start finish lv
            | otherwise = False

genMove :: Position -> Directions -> Level -> [((Position, Directions), Level)]
genMove pos North lvl@(L board) = if (checkEmptySpace ((fst pos) - 1, (snd pos)) lvl) && (checkStart pos lvl) 
                                   then [((pos, North), (addCell (emptySpace, pos) (addCell ((board A.! pos), ((fst pos) - 1, (snd pos))) lvl)))]
                                   else []
genMove pos South lvl@(L board) = if (checkEmptySpace ((fst pos) + 1, (snd pos)) lvl) && (checkStart pos lvl) 
                                   then [((pos, South), (addCell (emptySpace, pos) (addCell ((board A.! pos), ((fst pos) + 1, (snd pos))) lvl)))]
                                   else []
genMove pos East lvl@(L board) = if (checkEmptySpace ((fst pos), (snd pos) + 1) lvl) && (checkStart pos lvl) 
                                   then [((pos, East), (addCell (emptySpace, pos) (addCell ((board A.! pos), ((fst pos), (snd pos) + 1)) lvl)))]
                                   else []
genMove pos West lvl@(L board) = if (checkEmptySpace ((fst pos), (snd pos) - 1) lvl) && (checkStart pos lvl) 
                                   then [((pos, West), (addCell (emptySpace, pos) (addCell ((board A.! pos), ((fst pos), (snd pos) - 1)) lvl)))]
                                   else []
instance ProblemState Level (Position, Directions) where
    successors lvl @(L board) = generate 0 0 (snd (A.bounds board)) lvl
        where
            generate line col right_bottom l@(L board2)
                | (line == (fst right_bottom) + 1) = []
                | (not (elem (board2 A.! (line, col)) startChars))
                  && (not (elem (board2 A.! (line, col)) winChars))
                  && ((board2 A.! (line, col)) /= emptySpace) = (genMove (line, col) North lvl) ++ 
                                                                         (genMove (line, col) South lvl) ++ 
                                                                         (genMove (line, col) East lvl) ++ 
                                                                         (genMove (line, col) West lvl) ++ 
                                                                         if col == (snd right_bottom) 
                                                                            then generate (line + 1) 0 right_bottom l
                                                                            else generate line (col + 1) right_bottom l
                | otherwise = if col == (snd right_bottom) 
                                then generate (line + 1) 0 right_bottom l
                                else generate line (col + 1) right_bottom l

    isGoal lvl@(L board) = wonLevel lvl
    reverseAction state
        | (snd (fst state)) == North = ((((fst (fst (fst state))) - 1, (snd (fst (fst state)))), South), (moveCell (fst (fst state)) North (snd state)))
        | (snd (fst state)) == South = ((((fst (fst (fst state))) + 1, (snd (fst (fst state)))), North), (moveCell (fst (fst state)) South (snd state)))
        | (snd (fst state)) == East = ((((fst (fst (fst state))), (snd (fst (fst state))) + 1), West), (moveCell (fst (fst state)) East (snd state)))
        | otherwise = ((((fst (fst (fst state))), (snd (fst (fst state))) - 1), East), (moveCell (fst (fst state)) West (snd state)))
