module Sudoku where

import qualified Data.Set as S
import qualified Data.Char as C
import qualified Data.List as L

--constants
width = 9 :: Int
height = 9 :: Int


--data types
data Square = Sq Int | Em deriving  (Eq)
type Board = [Square]


--solve using a depth first search of the possible values to put in the squares
--returns Nothing iff the board is unsolvable
dfsSolve :: Board -> Maybe Board
dfsSolve b = case (L.elemIndex Em b) of
      (Nothing) -> if checkAll b then Just b else Nothing
      (Just i) -> firstNotNothing $
          map (\x -> dfsSolve $ replaceIndex i (Sq x) b) (reduceByAll i b)



--convience functions:
--safe access
(!!!) :: [a] -> Int -> Maybe a
(!!!) l i = if length l > i then Just (l !! i) else Nothing
infixl 9 !!!

--replace element in array
replace :: Eq a => a -> [a] -> [a] -> [a]
replace remove add = foldr (\x acc -> if x == remove then add++acc else x:acc)  []


--arithmetic sequence
arith :: Int -> Int -> [Int]
arith start n = start:(arith (start+n) n)


--take from list until an Empty value is reached
takeUntilEmpty :: [Maybe a] -> [a]
takeUntilEmpty (Nothing : xs) = []
takeUntilEmpty ((Just x) : xs) = x : (takeUntilEmpty xs)
takeUntilEmpty _ = []

--take indices from array, if they exist
takeIndices :: [a] -> [Int] -> [a]
takeIndices xs ins = takeUntilEmpty $ map (xs !!!) ins

--make array of value and the next two integers
and2 :: Int -> [Int]
and2 start = [start .. start+2]

--replace an index in an array note: not safe to call with non existant index
replaceIndex :: Int -> a -> [a] -> [a]
replaceIndex 0 ele ls = ele : (tail ls)
replaceIndex i ele ls = (\(f, s) -> f ++ (ele : (tail s))) (splitAt (i) ls)

--get the first value that is not Nothing
firstNotNothing :: [Maybe a] -> Maybe a
firstNotNothing [] = Nothing
firstNotNothing (Nothing : xs) = firstNotNothing xs
firstNotNothing (x : xs) = x

--turn a Square into an Int
sqToInt :: Square -> Int
sqToInt (Sq i) = i
sqToInt Em = 0

--turn list of squares into a set of Ints
squaresToSet :: [Square] -> S.Set Int
squaresToSet = S.fromList . (map sqToInt)




--solution algorithm functions and definitions:

--the array has all the numbers needed
containsAll :: [Square] -> Bool
containsAll sqs = S.fromList (map sqToInt sqs) == S.fromList [1 .. 9]

--the indices of rows, columns, and boxes (all the things that need to contain 1-9)
rows = map (\start -> [start .. (start + width - 1)]) (take width (arith 0 width))
cols = map (\start -> take height $ arith start height) (head rows)
boxes = map (\start ->
              (and2 start) ++ (and2 (start + width)) ++ (and2 (start + width *2)))
            [0, 3, 6, 27, 30, 33, 54, 57, 60]
allGroups = rows ++ cols ++ boxes


--check every row, column, and box to make sure that they contain 1-9
checkAll board = foldr (&&) True (map containsAll (map (takeIndices board) allGroups))


--reduce the list of possible values that a square can be by looking at its row
reduceByRow :: Int -> Board -> [Int]
reduceByRow i b = let row = head (filter (elem i) rows)
                      rowNums = (S.fromList . (map sqToInt) . (takeIndices b) $ row) in
                      filter (\x -> not $ S.member x rowNums) [1 .. 9]

--reduce the list of possible values that a square can be by looking at all possible conflicts
reduceByAll :: Int -> Board -> [Int]
reduceByAll i b = let neighbors = L.concat (filter (elem i) allGroups)
                      neighborNums = (S.fromList . (map sqToInt) . (takeIndices b) $ neighbors) in
                      filter (\x -> not $ S.member x neighborNums) [1 .. 9]




--string functions:

instance Show Square where
  show Em = "_"
  show (Sq i) = show i


rowToStr :: [Square] -> String
rowToStr = (L.intersperse ' ') . (map (head . show))

ppBoard :: Board -> String
ppBoard b = let rs = map (rowToStr . (takeIndices b)) rows in
                              L.concat (L.intersperse "\n" rs)

toSquare :: Char -> Square
toSquare c
      | C.isDigit c = Sq $ C.digitToInt c
      | otherwise = Em

toBoard :: String -> Board
toBoard = (map toSquare) . (replace '\n' "") . (replace ' ' "")
