module Main where

import qualified Data.Set as S
import qualified Data.Char as C
import qualified Data.List as L
width = 9 :: Int
height = 9 :: Int

(!!!) :: [a] -> Int -> Maybe a
(!!!) l i = if length l > i then Just (l !! i) else Nothing
infixl 9 !!!



main = putStrLn $ maybe "Fail" ppBoard (naiveRecursiveSolve testBoard2)


replace :: Eq a => a -> [a] -> [a] -> [a]
replace remove add = foldr (\x acc -> if x == remove then add++acc else x:acc)  []

data Square = Sq Int | Em deriving  (Eq)


instance Show Square where
  show Em = "_"
  show (Sq i) = show i

type Board = [Square]

rowToStr :: [Square] -> String
rowToStr = (L.intersperse ' ') . (map (head . show))

ppBoard :: Board -> String
ppBoard b = let rs = map (rowToStr . (takeIndices b)) rows in
                L.concat (L.intersperse "\n" rs)


sqToInt :: Square -> Int
sqToInt (Sq i) = i
sqToInt Em = 0

containsAll :: [Square] -> Bool
containsAll sqs = S.fromList (map sqToInt sqs) == S.fromList [1 .. 9]


arith :: Int -> Int -> [Int]
arith start n = start:(arith (start+n) n)

takeUntilEmpty :: [Maybe a] -> [a]
takeUntilEmpty (Nothing : xs) = []
takeUntilEmpty ((Just x) : xs) = x : (takeUntilEmpty xs)
takeUntilEmpty _ = []

takeIndices :: [a] -> [Int] -> [a]
takeIndices xs ins = takeUntilEmpty $ map (xs !!!) ins

and2 :: Int -> [Int]
and2 start = [start .. start+2]

rows = map (\start -> [start .. (start + width - 1)]) (take width (arith 0 width))
cols = map (\start -> take height $ arith start height) (head rows)
boxes = map (\start ->
              (and2 start) ++ (and2 (start + width)) ++ (and2 (start + width *2)))
            [0, 3, 6, 27, 30, 33, 54, 57, 60]

allGroups = rows ++ cols ++ boxes

checkAll board = foldr (&&) True (map containsAll (map (takeIndices board) allGroups))



replaceIndex :: Int -> a -> [a] -> [a]
replaceIndex 0 ele ls = ele : (tail ls)
replaceIndex i ele ls = (\(f, s) -> f ++ (ele : (tail s))) (splitAt (i) ls)

firstNotNothing :: [Maybe a] -> Maybe a
firstNotNothing [] = Nothing
firstNotNothing (Nothing : xs) = firstNotNothing xs
firstNotNothing (x : xs) = x

squaresToSet :: [Square] -> S.Set Int
squaresToSet = S.fromList . (map sqToInt)

reduceByRow :: Int -> Board -> [Int]
reduceByRow i b = let row = head (filter (elem i) rows)
                      rowNums = (S.fromList . (map sqToInt) . (takeIndices b) $ row) in
                      filter (\x -> not $ S.member x rowNums) [1 .. 9]

reduceByAll :: Int -> Board -> [Int]
reduceByAll i b = let neighbors = L.concat (filter (elem i) allGroups)
                      neighborNums = (S.fromList . (map sqToInt) . (takeIndices b) $ neighbors) in
                      filter (\x -> not $ S.member x neighborNums) [1 .. 9]



naiveRecursiveSolve :: Board -> Maybe Board
naiveRecursiveSolve b = case (L.elemIndex Em b) of
      (Nothing) -> if checkAll b then Just b else Nothing
      (Just i) -> firstNotNothing $
          map (\x -> naiveRecursiveSolve $ replaceIndex i (Sq x) b) (reduceByAll i b)





emptyBoard = replicate 81 Em


toSquare :: Char -> Square
toSquare c
      | C.isDigit c = Sq $ C.digitToInt c
      | otherwise = Em

toBoard :: String -> Board
toBoard = (map toSquare) . (replace '\n' "") . (replace ' ' "")

testBoard = toBoard "___26_7_1 68__7__9_ 19___45__ 82_1___4_ __46_29__ _5___3_28 __93___74 _4__5__36 7_3_18___"
testBoard2 = toBoard "_2_______ ___6____3 _74_8____ _____3__2 _8__4__1_ 6__5_____ ____1_78_ 5____9___ _______4_"
testBoard3 = toBoard "435269781 682571493 197834562 826195347 374682915 951743628 519326874 248957136 763418259"
