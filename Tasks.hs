{-
    PP Project 2021

    This is where you will write the implementation for the given tasks.
    You can add other modules aswell.
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Tasks where

import Dataset
import Text.Printf
import Data.List
import Data.Function 
import Data.Ord
import Numeric 
import Data.Maybe
import Text.Read
import Data.Array

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]


{-
    TASK SET 1
-}

-- Task 1
rowToFloat :: Row -> [Float]
rowToFloat = map (\x -> if x /= "" then read x::Float else 0)

getFloatGrade :: [Float] -> Float
getFloatGrade l = (sum (take 6 l)) / 4 + last l

getStringGrade :: Row -> String
getStringGrade = (printf "%.2f") . (getFloatGrade) . (rowToFloat)

getPairs :: Row -> Row
getPairs r = ((head r) : []) ++ [getStringGrade (tail r)]

tableContent :: Table -> Table
tableContent = (map (getPairs)) . (tail)

compute_exam_grades :: Table -> Table
compute_exam_grades t = ["Nume", "Punctaj Exam"] : tableContent t

-- Task 2
-- Number of students who have passed the exam:
toFloatList :: Table -> [Float]
toFloatList = (rowToFloat) . (map (head)) . (map (tail))

count :: [Float] -> Int
count = foldr (\x acc -> if(x >= 2.5) then acc + 1 else acc) 0

get_passed_students_num :: Table -> Int
get_passed_students_num = (count) . (toFloatList) . (tableContent)

-- Percentage of students who have passed the exam:
getStudNum :: Table -> Int
getStudNum = (length) . (tableContent)

get_passed_students_percentage :: Table -> Float
get_passed_students_percentage t = (fromIntegral(get_passed_students_num t)) / (fromIntegral(getStudNum t))

-- Average exam grade
getSum :: Table -> Float
getSum = (sum) . (toFloatList) . (tableContent)

get_exam_avg :: Table -> Float
get_exam_avg t = (getSum t) / (fromIntegral (getStudNum t))

-- Number of students who gained at least 1.5p from homework:
hwTotalList :: Table -> [Float]
hwTotalList = (map sum) . (map (take 3)) .
 (map tail) .
  (map rowToFloat) .
   (map tail) .
    (tail)

hwCount :: [Float] -> Int
hwCount = foldr (\x acc -> if(x >= 1.5) then acc + 1 else acc) 0

get_passed_hw_num :: Table -> Int
get_passed_hw_num = (hwCount) . (hwTotalList)

-- Task 3
qTotalList :: Table -> [Float]
qTotalList = (map sum) .
 (transpose) .
  (map rowToFloat) .
   (map init) .
    (map tail) .
     (tail)

qAverageList :: Table -> [Float]
qAverageList t = map (/fromIntegral(getStudNum t)) $ qTotalList t

toTwoDecimals :: [Float] -> [String]
toTwoDecimals = map (\x -> showFFloat (Just 2) x "")

toOneDecimal :: [Float] -> [String]
toOneDecimal = map (\x -> showFFloat (Just 1) x "")

qAverageListString :: Table -> [String]
qAverageListString = (toTwoDecimals).(qAverageList)

get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs t = ["Q1", "Q2", "Q3", "Q4", "Q5", "Q6"] : [qAverageListString t]

-- Task 4
rowToInt :: Row -> [Int]
rowToInt = map (\x -> if x /= "" then read x::Int else 0)

countZeros :: [Int] -> Int
countZeros = foldr (\x acc -> if (x == 0) then acc + 1 else acc) 0

countOnes :: [Int] -> Int
countOnes = foldr (\x acc -> if (x == 1) then acc + 1 else acc) 0

countTwos :: [Int] -> Int
countTwos = foldr (\x acc -> if (x == 2) then acc + 1 else acc) 0

getZerosList :: Table -> [String]
getZerosList = (map show) .
 (map (countZeros)) .
  (transpose) .
   (map rowToInt) .
    (map init) .
     (map tail) .
      (tail)

getOnesList :: Table -> [String]
getOnesList = (map show) .
 (map (countOnes)) .
  (transpose) .
   (map rowToInt) .
    (map init) .
     (map tail) .
      (tail)

getTwosList :: Table -> [String]
getTwosList = (map show) .
 (map (countTwos)) .
  (transpose) .
   (map rowToInt) .
    (map init) .
     (map tail) .
      (tail)

getFrequencies :: Table -> Table
getFrequencies t =  ((transpose) $ ((getZerosList t) : (getOnesList t) : (getTwosList t) : []))

addQ :: Table -> Table
addQ = zipWith (++) [["Q1"],["Q2"],["Q3"],["Q4"],["Q5"],["Q6"],["Q7"]]

get_exam_summary :: Table -> Table
get_exam_summary t = ["Q", "0", "1", "2"] : addQ (getFrequencies t)

-- Task 5
toTuple :: [t] -> (t, t)
toTuple [x,y] = (x,y)

myfst (x,y,z) = x
mysnd (x,y,z) = y
mytrd (x,y,z) = z

insertTuple op (x,y) = (x,y,(op x y))

toList :: RealFloat a => (String, a) -> [String]
toList (x,y) = [x, showFFloat (Just 2) (y) ""]

compareTuple :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
compareTuple t1 t2 = if snd(t1) == snd(t2) 
                        then if fst(t1) > fst(t2) 
                            then GT 
                        else LT 
                    else if snd(t1) > snd(t2) 
                        then GT else LT
compare3Tuple :: (Ord a, Ord b, Ord c) => (a, b, c) -> (a, b, c) -> Ordering
compare3Tuple t1 t2 = if mytrd(t1) > mytrd(t2)
                            then GT
                        else if mytrd(t1) < mytrd(t2)
                            then LT
                        else EQ

sortTuple :: (Ord b, Ord a) => [(a, b)] -> [(a, b)]
sortTuple = sortBy (compareTuple)

sort3Tuple :: (Ord b, Ord a, Ord c) => [(a, b, c)] -> [(a, b, c)]
sort3Tuple = sortBy (compare3Tuple)

convertGrade :: (String, String) -> (String, Float)
convertGrade t = (fst t, (read (snd t)::Float)) 

getSorted :: Table -> Table
getSorted = (map toList) .
 (sortTuple) .
  (map convertGrade) .
   (map (toTuple)) .
    (tail) .
     (compute_exam_grades)

get_ranking :: Table -> Table
get_ranking t = ["Nume", "Punctaj Exam"] : getSorted t

-- Task 6
to3Tuple :: [[Char]] -> ([Char], Float, Float)
to3Tuple [x,y,z] = (x, read y::Float, read z::Float)

to4Tuple :: [[Char]] -> ([Char], Float, Float, Float)
to4Tuple [x,y,z,t] = (x, read y::Float, read z::Float, read t::Float)

to4List :: (RealFloat a2, RealFloat a1, RealFloat a) => (String, a2, a1, a) -> [String]
to4List (x,y,z,t) = [x, showFFloat (Just 2) (y) "", showFFloat (Just 2) (z) "", showFFloat (Just 2) (t) ""]

computeDiff :: (String, Float, Float) -> (String, Float, Float, Float)
computeDiff (name, grade1, grade2) = 
    if grade1 > grade2 
        then (name, grade1 - grade2, grade2, grade1 - 2 * grade2) 
        else (name, grade1 - grade2, grade2, 2 * grade2 - grade1)

getWrittenScore :: Table -> Table
getWrittenScore = (tail) . (map(:[])) . (map (last))

getInterview :: Table -> Table
getInterview = (tail) . (compute_exam_grades)

concatWrittenInterview :: [[[Char]]] -> [([Char], Float, Float)]
concatWrittenInterview t = (map (to3Tuple)) $ (zipWith (++) (getInterview t) (getWrittenScore t))

sort4Tuple :: (Ord a, Ord a1) => [(a1, t3, t2, a)] -> [(a1, t3, t2, a)]
sort4Tuple = sortBy (compare4Tuple)

compare4Tuple :: (Ord a1, Ord a) => (a1, t3, t2, a) -> (a1, t1, t, a) -> Ordering
compare4Tuple (name1, interv1, writ1, diff1) (name2, interv2, writ2, diff2) =
 if diff1 == diff2 
    then if name1 > name2 
        then GT 
    else LT 
 else if diff1 > diff2 
     then GT 
 else LT

concatDiff :: [[[Char]]] -> [([Char], Float, Float, Float)]
concatDiff = (map (computeDiff)) . (concatWrittenInterview)

addHead :: [String] -> Table -> Table
addHead table = (:) table

get_exam_diff_table :: Table -> Table
get_exam_diff_table = (addHead ["Nume", "Punctaj interviu", "Punctaj scris", "Diferenta"]) .
 (map to4List) .
  (sort4Tuple) .
   (map to4Tuple) .
    (map to4List) .
     (sort4Tuple) .
      (concatDiff)


{-
    TASK SET 2
-}
-- Prerequisite
-- separa un string in lista de stringuri dupa un caracter separator
-- foldr pe string cu acumulator initial lista cu un singur element: stringul vid
splitBy :: Char -> String -> [String]
splitBy separator = foldr (\char accumulator@(x:xs) -> -- primeste caracterul curent si un acumulator
    if char == separator                               -- am intalnit separatorul
        then "" : accumulator                          -- adauga un string nou gol la acumulator
    else (char : x) : xs) [""]                         -- adauga caracterul curent la primul string din acumulator

-- completeaza notele care lipsesc cu ""
-- foldr pe string cu acumulator initial stringul vid
completeWithZeros :: String -> String
completeWithZeros = foldr (\char accumulator ->        -- primeste caracterul curent si un acumulator
    if char == ',' && accumulator == ""                -- avem virgula la finalul stringului
        then [char] ++ "" ++ accumulator               -- pune "" la finalul stringului
    else if char == ',' && head accumulator == ','     -- avem doua virgule consecutive (nota lipsa)
        then [char] ++ "" ++ accumulator               -- pune "" intre virgule
    else [char] ++ accumulator) ""                     -- nu avem succesiune de 2 virgule

read_csv :: CSV -> Table
read_csv csv = map (splitBy ',') $ (map completeWithZeros) $ (splitBy '\n' csv)

write_csv :: Table -> CSV
write_csv table = (intercalate "\n") $ map (intercalate ",") table

-- Task 1
-- as_list, dar primeste ca parametru tabelul transpus
as_list_from_trans :: String -> Table -> [String]
as_list_from_trans _ [] = []
as_list_from_trans columnName table = 
    if head (head table) == columnName
        then tail (head table)
    else as_list_from_trans columnName (tail table)

as_list :: String -> Table -> [String]
as_list columnName t = as_list_from_trans columnName (transpose t)

-- Task 2
-- returneaza indexul unei coloane dintr-un tabel pe baza numelui coloanei
getIndexColumnByName :: String -> Table -> Int
getIndexColumnByName columnName table = head $ elemIndices columnName (head table)

-- compara elementele a 2 coloane, al caror index este obtinut cu getIndexColumnByName
-- daca cele 2 elemente sunt egale, se compara numele coloanelor
compareColumns:: String -> Table -> [String] -> [String] -> Ordering
compareColumns columnName table = (\e1 e2 ->
    if compare (e1 !! (getIndexColumnByName columnName table)) 
               (e2 !! (getIndexColumnByName columnName table)) == EQ
        then compare (e1 !! 0) (e2 !! 0)
    else if (not $ (isNothing (readMaybe (e1 !! (getIndexColumnByName columnName table))::Maybe Double))) && 
            (not $ (isNothing (readMaybe (e2 !! (getIndexColumnByName columnName table))::Maybe Double)))
        then compare (fromJust $ (readMaybe (e1 !! (getIndexColumnByName columnName table))::Maybe Double))
                     (fromJust $ (readMaybe (e2 !! (getIndexColumnByName columnName table))::Maybe Double))
    else compare (e1 !! (getIndexColumnByName columnName table))
                 (e2 !! (getIndexColumnByName columnName table)))

tsort :: String -> Table -> Table
tsort columnName table = (head table) : sortBy (compareColumns columnName table) (tail table)

-- Task 3
-- aplica o functie data ca parametru pe fiecare element dintr-un rand
mymap :: (Value -> Value) -> Row -> Row
mymap f [] = []
mymap f row@(x:xs) = (f x) : (mymap f xs)  

-- aplica o functie data ca parametru pe fiecare element dintr-un tabel
vmap :: (Value -> Value) -> Table -> Table
vmap f = map (mymap f)

-- Task 4
-- aplica map de functie pe tabel si schimba headerul
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap op header table = header : (map op (tail table))

-- calculeaza suma punctelor la teme minus laboratorul
getTotalPoints :: Row -> Float
getTotalPoints row = (sum $ rowToFloat (tail row)) - head (rowToFloat (tail row))

-- formeaza perechea nume - valoare
get_hw_grade_total :: Row -> Row
get_hw_grade_total row = (head row) : [(showFFloat (Just 2) (getTotalPoints row) "")]  

-- Task 5
vunion :: Table -> Table -> Table
vunion t1 t2 = if head t1 == head t2 then t1 ++ (tail t2) else t1

-- Task 6
-- Daca tabelele au numar egal de linii, se concateneaza transpusele si tabelul rezultat se
-- transpune iar
-- Daca tabelele au numar diferit de linii, se verifica care are mai putine si se completeaza
-- cu diferenta. Lungimea unei linii va fi data de lungimea primului rand din tabelul mai mic
hunion :: Table -> Table -> Table
hunion t1 t2 =
    if length t1 == length t2 
      then transpose (transpose t1 ++ transpose t2) 
    else if length t1 < length t2
      then transpose (transpose (t1 ++ take (length t2 - length t1) (repeat (take (length (head t1)) (repeat "")))) ++ transpose t2)
    else transpose (transpose t1 ++ transpose (t2 ++ take (length t1 - length t2) (repeat (take (length (head t2)) (repeat "")))))

-- Task 7
-- sterge dintr-un rand elementul de la indexul dat ca parametru
-- va fi apelat cu indexul rezultat din getIndexColumnByName
removeElementFromRow :: Row -> Int -> Row
removeElementFromRow row index = take index row ++ drop (1 + index) row

-- formeaza headerul pentru tjoin
getHeaderTJoin :: String -> Table -> Table -> Row
getHeaderTJoin key t1 t2 = (head t1) ++ (removeElementFromRow (head t2) (getIndexColumnByName key t2))

-- formeaza tabelul cu headerul si cu intrarile lui t1
getFirstHalf :: String -> Table ->Table -> Table
getFirstHalf key t1 t2 = (getHeaderTJoin key t1 t2) : (tail t1)

-- primeste o cheie si un tabel si returneaza indexul randului pe care se afla acea cheie
getRowIndex :: String -> Table -> Int
getRowIndex key [] = -1
getRowIndex key table@(r:rs) = if key `elem` r then head (elemIndices r table) else 1 + (getRowIndex key rs)

-- se lucreaza direct pe t1
-- formeaza randuri din intrarile care se afla in t1, dar nu si in t2
-- se parcurge t1 cu un map si se verifica fiecare linie daca exista si in t2
-- daca exista se adauga elementele din t2 la randul din t1 cu paddingul corespunzator
getTableOne :: String -> Table -> Table -> Table
getTableOne key t1 t2 = (getHeaderTJoin key t1 t2) : (tail $ 
  map (\r -> 
    if (r !! (getIndexColumnByName key t1)) `elem` (as_list key t2) 
      then r ++ (removeElementFromRow (t2 !! (getRowIndex (r !! (getIndexColumnByName key t1)) t2)) (getIndexColumnByName key t1)) 
      else r ++ take (length (removeElementFromRow (head t2) (getIndexColumnByName key t2))) (repeat "")) t1)

-- formeaza un tabel cu intrarile care se gasesc in t2, dar nu si in t1 prin foldarea lui t2
getTableTwo :: String -> Table -> Table -> Table
getTableTwo key t1 t2 = foldr (\r acc -> 
  if (r !! (getIndexColumnByName key t2)) `notElem` (as_list key t1)
    then ([head r] ++ take (abs ((length (head t1)) - (length (head t2)))) (repeat "") ++ (tail r)) : acc
  else acc) [] t2

-- se concateneaza cele 2 jumatati de tabel
tjoin :: String -> Table -> Table -> Table
tjoin key t1 t2 = (getTableOne key t1 t2) ++ (tail (getTableTwo key t1 t2)) 


-- Task 8
-- Aplic list comprehension cu opearorul op pe tail de tabel 1 si tail de tabel 2, apoi concatenez headerul
cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian op header t1 t2 = header : [op x y | x <- (tail t1), y <- (tail t2)]

-- Task 9
-- Se face fold pe tabelul transpus, verificand daca primul element din fiecare rand face parte
-- din lista de nume de coloane data ca parametru. Daca da, adauga-l la noul tabel 
projection_from_trans :: [String] -> Table -> Table
projection_from_trans colNames table = transpose $ foldr (\row acc -> if (head row) `elem` colNames then row : acc else acc) [[]] table

projection :: [String] -> Table -> Table
projection colNames table = projection_from_trans colNames (transpose table)



{-
    TASK SET 3
-}
data Query =
    FromCSV CSV
    | ToCSV Query
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query
    | Graph EdgeOp Query
 
-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

data QResult = CSV CSV | Table Table | List [String]


--3.1
instance Show QResult where
    show (CSV csv) = show csv
    show (List list) = show list
    show (Table table) = write_csv table

--3.2
class Eval a where
    eval :: a -> QResult


-- Se ia un acumulator de forma lista de randuri si un rand dat.
-- Se parcurge tabelul de la randul dat in jos si pentru fiecare pereche row - rand_tabel se verifica cu edgeop daca exista muchie intre ele
-- Daca exista muchie atunci se adauga la acumulator un rand de forma nume_nume_valoare si se inainteaza in recursivitate pe tabel.
-- Daca nu exista muchie atunci la acumulator se adauga doar ceea ce rezulta din inaintarea in recursivitate.  
body_table edgeop _ _ [] = []
body_table edgeop acc row t@(r:rss) = 
    if not(isNothing $ edgeop row r) 
        then if (row !! 0) < (r !! 0) 
            then ((acc ++ [row !! 0] ++ [r !! 0] ++ [fromJust (edgeop row r)]) : []) ++ (body_table edgeop acc row rss)
        else ((acc ++ [r !! 0] ++ [row !! 0] ++ [fromJust (edgeop row r)]) : []) ++ (body_table edgeop acc row rss)
    else (acc : []) ++ (body_table edgeop acc row rss)

-- Se inainteaza in recursivitate pe intreg tabelul pentru a forma intrarile de forma nume_nume_valoare pentru toate muchiile posibile.
wrapper_body_table edgeop _ [] = []
wrapper_body_table edgeop acc t@(r:rs) = (body_table edgeop acc r rs) ++ (wrapper_body_table edgeop acc rs)

instance Eval Query where
    eval (FromCSV csv) = Table (read_csv csv)
    eval (ToCSV query) = CSV (show $ eval query)
    eval (AsList colname query) = List (as_list colname (read_csv $ show $ eval query))
    eval (Sort colname query) = Table (tsort colname (read_csv $ show $ eval query))
    eval (ValueMap op query) = Table (vmap op (read_csv $ show $ eval query))
    eval (RowMap op colnames query) = Table (rmap op colnames (read_csv $ show $ eval query))
    eval (VUnion query1 query2) = Table (vunion (read_csv $ show $ eval query1) (read_csv $ show $ eval query2))
    eval (HUnion query1 query2) = Table (hunion (read_csv $ show $ eval query1) (read_csv $ show $ eval query2))
    eval (TableJoin colname query1 query2) = Table (tjoin colname (read_csv $ show $ eval query1) (read_csv $ show $ eval query2))
    eval (Cartesian op colnames query1 query2) = Table (cartesian op colnames (read_csv $ show $ eval query1) (read_csv $ show $ eval query2))
    eval (Projection colnames query) = Table (projection colnames (read_csv $ show $ eval query))
--3.4
-- Se face fold pe tabel, rand cu rand, verificanduse daca randul curent este evaluat de feval la True.
-- Daca da, atunci randul curent se concateneaza la acumulatorul foldului, daca nu este intors acumulatorul.
    eval (Filter cond query) = Table ((head $ read_csv $ show $ eval query) : (foldr (\row acc ->
         if (feval (head $ read_csv $ show $ eval query) cond) row == True
          then [row] ++ acc
        else acc) [] (tail $ read_csv $ show $ eval query)))
--3.5
    eval (Graph edgeop query) = Table (filter (not . null) $ (addHead ["From","To","Value"]) $ (wrapper_body_table edgeop [] (tail $ read_csv $ show $ eval query)))
                                        
--3.6
-- Numara cate note corespund intre cele 2 randuri r1 si r2
countingQuestions :: Row -> Row -> Int
countingQuestions _ [] = 0
countingQuestions [] _ = 0
countingQuestions r1@(grade1:grades1) r2@(grade2:grades2) = if grade1 == grade2 then 1 + (countingQuestions grades1 grades2) else countingQuestions grades1 grades2

-- Calculeaza distanta
distance :: Row -> Row -> Maybe Value
distance r1 r2 
    | countingQuestions r1 r2 >= 5 = Just (show $ countingQuestions r1 r2)
    | otherwise = Nothing

similarities_query :: Query
similarities_query = Sort "Value" $ Graph distance $ Filter (FNot (Eq "Email" "")) $ FromCSV $ lecture_grades_csv

--3.3
data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

type FilterOp = Row -> Bool

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

instance FEval Float where
    feval colnames (Eq colname ref) = (\row -> if row !! (head $ elemIndices colname colnames) == printf "%.2f" ref then True else False)
    feval colnames (Lt colname ref) = (\row -> if row !! (head $ elemIndices colname colnames) < printf "%.2f" ref then True else False)
    feval colnames (Gt colname ref) = (\row -> if row !! (head $ elemIndices colname colnames) > printf "%.2f" ref then True else False)
    feval colnames (In colname list) = (\row -> if row !! (head $ elemIndices colname colnames) `elem` map show (map round list) then True else False)
    feval colnames (FNot cond) = (\row -> if not ((feval colnames cond) row) == True then True else False)
    feval colnames (FieldEq colname1 colname2) = (\row -> if row !! (head $ elemIndices colname1 colnames) == row !! (head $ elemIndices colname2 colnames) then True else False)

instance FEval String where
    feval colnames (Eq colname ref) = (\row -> if row !! (head $ elemIndices colname colnames) == ref then True else False)
    feval colnames (Lt colname ref) = (\row -> if row !! (head $ elemIndices colname colnames) < ref then True else False)
    feval colnames (Gt colname ref) = (\row -> if row !! (head $ elemIndices colname colnames) > ref then True else False)
    feval colnames (In colname list) = (\row -> if row !! (head $ elemIndices colname colnames) `elem` list then True else False)
    feval colnames (FNot cond) = (\row -> if not ((feval colnames cond) row) == True then True else False)
    feval colnames (FieldEq colname1 colname2) = (\row -> if row !! (head $ elemIndices colname1 colnames) == row !! (head $ elemIndices colname2 colnames) then True else False)



{-
    TASK SET 4
-}

-- 4.1

-- extrage coloana cu numele colname din csv si o returneaza sub forma de tabel query
getColumnQuery :: String -> CSV -> Query
getColumnQuery colname csv = Projection [colname] $ FromCSV csv

-- returneaza o coloana sub forma tabel (folosit pt a lua numele din tabelul de referinta)
getColumnAsTable colname csv = read_csv $ show $ eval $ getColumnQuery colname csv

-- verifica daca un nume este egal cu numele ref
-- folosita pt searchForMatches
searchForMatch ref table = foldr (\r acc -> if (r !! 0) == ref then [r] ++ acc else acc) [] table

-- cauta numele care au corespondent. Toate numele bagate in lista asta urmeaza sa fie sterse din T si din Ref
searchForMatches [] table = []
searchForMatches refTable@(x:xs) typoTable = (searchForMatch (head x) typoTable) ++ (searchForMatches xs typoTable)

-- sterge numele care au corespondent din table
deleteMatches _ [] _ = []
deleteMatches refTable table@(x:xs) acc = if x `elem` refTable then acc ++ (deleteMatches refTable xs acc) else [x] ++ acc ++ (deleteMatches refTable xs acc)   

-- returneaza tabelul de nume care nu au corespondent intre T si Ref cu headerul "colname"
final colname refTable table acc = addHead [colname] $ (deleteMatches refTable table acc)

-- calcularea distantei :

-- transformam stringurile in arrays pentru indexare mai eficienta
stringToArray string = Data.Array.listArray (1, (length string)) string

-- Calculeaza distanta dintre 2 stringuri
-- Functia auxiliara editDistance calculeaza distanta dintre sufixele de lungime i si j
-- Parcurgem cele 2 stringuri litera cu litera incercand cele 3 operatii posibile: stergere, adaugare, modificare si
-- alegem actiunea care minimizeaza distanta
-- Fiecare valoare a distantei calculata intre 2 sufixe se pastreaza intr-o matrice ds pentru a nu fi nevoiti sa
-- calculam acea distanta de fiecare data cand vrem sa alegem distanta minima
-- inspiratie algoritm: curs Lazy Evaluation in Haskell ppcarte
computeDistance string1 string2 = editDistance (length string1) (length string2)
  where editDistance i 0 = i -- am ramas fara litere in stringul 2 => distanta = nr litere ramase in string 1
        editDistance 0 j = j -- am ramas fara litere in stringul 1 => distanta = nr litere ramase in string 2
        editDistance i j = if (stringToArray string1) ! i ==  (stringToArray string2) ! j then ds ! (i - 1, j - 1) 
                            else minimum [ ds ! (i - 1, j) + 1, ds ! (i, j - 1) + 1, ds ! (i - 1, j - 1) + 1]
        ds = Data.Array.listArray ((0, 0), ((length string1), (length string2)))
               [editDistance i j | (i, j) <- Data.Array.range ((0, 0), ((length string1), (length string2)))]

-- combina coloanele typos si referinta intr-un singur tabel
getCombinations colname typoCSV refCSV = read_csv $ show $ eval $ Cartesian (++) ["Typo","Ref"] (getColumnQuery colname typoCSV) (getColumnQuery colname refCSV)

-- se converteste tabelul obtinut prin getCombinations la o lista de trupluri cu fiecare element de forma:
-- (nume_cu_typo, nume_de_referinta, distanta)
createTupleList table = map (\t -> insertTuple computeDistance t) (map (toTuple) table)

-- din lista de tupluri se pastreaza pentru numele "ref", tuplul cu distanta cea mai mica fata de el
keepRefMinimum tupleList ref = head $ sort3Tuple $ foldr (\t acc -> if myfst t == ref then acc ++ [t] else acc) [] tupleList

-- se parcurge tabelul si se pastreaza in lista de tupluri, pentru fiecare nume, tuplul cu distanta cea mai mica
keepMinimumList [] _ = []
keepMinimumList table@(x:xs) tupleList = [keepRefMinimum tupleList (head x)]  ++ keepMinimumList xs tupleList

-- inlocuieste in randul row elementul de la index cu newRef
-- este folosita pentru a inlocui numele cu typos cu numele bun
replaceNth _ _ [] = []
replaceNth index newRef row@(r:rs) = if index == 0 then newRef : rs else r : replaceNth (index - 1) newRef rs

-- parcurge lista de tupluri si verifica pe rand daca primul element din tuplu (numele cu typos) corespunde
-- cu numele de la indexul dat de colname de pe randul r
-- daca da, se inlocuieste numele gasit cu al doilea element din tuplu (numele fara typos)
-- daca nu, se merge inainte in recursivitate pe lista de tupluri
replaceOneRef table r [] _ = r
replaceOneRef table r tupleList@(t:ts) colname = if r !! (getIndexColumnByName colname table) == (myfst t)
                                             then (replaceNth (getIndexColumnByName colname table) (mysnd t) r)
                                            else replaceOneRef table r ts colname

-- se mapeaza pe tabelul cu typos functia anterioara de replace pentru a corecta toate numele cu typos din tabel
replaceAllRefs colname table tupleList = (map (\r  -> (replaceOneRef table r tupleList colname)) table)

correct_table :: String -> CSV -> CSV -> CSV
correct_table colname typoCSV refCSV = write_csv $ replaceAllRefs
 colname
  (read_csv typoCSV) 
  (keepMinimumList
   (tail
    (final
     colname 
     (searchForMatches 
        (getColumnAsTable colname refCSV) 
        (getColumnAsTable colname typoCSV)) 
     (getColumnAsTable colname typoCSV) 
     [])) 
   (createTupleList 
    (getCombinations 
        colname 
        (write_csv 
            (final 
                colname 
                (searchForMatches 
                    (getColumnAsTable colname refCSV) 
                    (getColumnAsTable colname typoCSV)) 
                (getColumnAsTable colname typoCSV) 
                [])) 
        (write_csv 
            (final 
                colname 
                (searchForMatches 
                    (getColumnAsTable colname refCSV) 
                    (getColumnAsTable colname typoCSV)) 
                (getColumnAsTable colname refCSV) 
                [])))))


-- 4.2
-- extrage tabel cu numele
names = tail $ read_csv $ show $ eval $ getColumnQuery "Nume" hw_grades_csv

-- extrage maparea nume-email
emailMapTable = tsort "Nume" $ read_csv $ correct_table "Nume" email_map_csv hw_grades_csv

-- extrage toate emailurile
allEmails = map (tail) emailMapTable

-- extrage emailurile de la curs
lectureEmails = map (:[]) $ sortBy (compare) $ filter (/= "") $ map (head) $ tail $ lecture_grades

-- extrage numele de la examen
examNames = map (:[]) $ sortBy (compare) $ map (head) $ tail $ exam_grades

-- tabelul cu notele de curs, sortat, fara header si fara randuri goale
sortedLectureGrades = tail $ foldr(\r acc -> if r !! 0 /= "" then [r] ++ acc else acc) [] (tsort "Email" $ lecture_grades)

-- tabeleul cu notele de la examen, sortat, fara header si fara randuri goale
sortedExamGrades = tail $ foldr(\r acc -> if r !! 0 /= "" then [r] ++ acc else acc) [] (tsort "Nume" $ exam_grades)

{-Punctaj teme-}
-- returneaza notele temelor ca lista de floats
getGradesFloats hwTable =  map (\r -> rowToFloat (tail r)) (tail hwTable) 

-- insumeaza notele temelor si returneaza o lista cu sumele "Punctaj Teme" ca stringuri
computeHwGradesList hwTable = toTwoDecimals $ map (\r -> foldr (\el acc -> acc + el) 0 r) $ getGradesFloats hwTable

-- lista de "Punctaj Teme" este transformata intr-un tabel pentru a se putea face ulterior hunion la tabelul mare
getHwGradesTable = map (:[]) (computeHwGradesList hw_grades)

getHwTable = tsort "Nume" $ addHead ["Nume", "Punctaj Teme"] $ hunion names getHwGradesTable

{-Punctaj Curs-}
getRowByEmail email = foldr(\r acc -> if (head r) == email then acc ++ r else acc) [] sortedLectureGrades

computeGradeForOne row = printf "%.2f" $ (2 * (sum $ rowToFloat (tail row))) / (fromIntegral $ length $ tail $ head $ lecture_grades)

-- nu toti studentii care au trimis teme au si punctaj de curs
getLectureEmailsTable = foldr (\r acc -> if r `elem` lectureEmails then r ++ acc else [""] ++ acc) [] (tail allEmails)

getLectureGradesTable = map (:[]) $ init $ foldr (\email acc -> if email /= "" then [computeGradeForOne (getRowByEmail email)] ++ acc else [""] ++ acc) [[]] getLectureEmailsTable

getHwAndLectureTable = addHead ["Nume", "Punctaj Teme", "Punctaj curs"] $ hunion (tail getHwTable) getLectureGradesTable

{-Punctaj Exam-}
getRowByName name = foldr(\r acc -> if (head r) == name then acc ++ r else acc) [] sortedExamGrades

computeExamForOne row = printf "%.2f" $ (sum (take 6 (rowToFloat (tail row))) / 4 + last (rowToFloat row))

getExamNamesTable = foldr (\r acc -> if r `elem` examNames then r ++ acc else [""] ++ acc) [] (sortBy (compare) names)

getExamGradesTable = map (:[]) $ init $ foldr (\name acc -> if name /= "" then [computeExamForOne (getRowByName name)] ++ acc else [""] ++ acc) [[]] getExamNamesTable

getHwLectureAndExamTable = addHead ["Nume", "Punctaj Teme", "Punctaj Curs", "Punctaj Exam"] $ hunion (tail getHwAndLectureTable) getExamGradesTable

{-Punctaj Total-}
getRowByNameG name tabel = foldr(\r acc -> if (head r) == name then acc ++ r else acc) [] tabel

hw_grade row = rowToFloat (tail row) !! 0
lecture_grade row = rowToFloat (tail row) !! 1
exam_grade row = rowToFloat (tail row) !! 2


computeTotalForOne row = if (hw_grade row) + (lecture_grade row) < 2.5 || (exam_grade row) < 2.5 then 4.0
                        else (min ((hw_grade row) + (lecture_grade row)) 5) + exam_grade row

getTotalTable = map (:[]) $ (toTwoDecimals) $ foldr (\r acc -> (computeTotalForOne r) : acc) [] (tail getHwLectureAndExamTable)

getFinalTable = addHead ["Nume", "Punctaj Teme", "Punctaj Curs", "Punctaj Exam", "Punctaj Total"] $ hunion (tail getHwLectureAndExamTable) getTotalTable

grades :: CSV -> CSV -> CSV -> CSV -> CSV
grades emailCSV hwCSV examCSV lectureCSV = write_csv $ getFinalTable


