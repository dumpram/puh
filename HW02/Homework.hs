module Main where

import Data.List 
import Data.List.Split
import System.Environment

-- TASK01

-- The function checks whether the matrix has all rows of equal length.
isWellFormed :: [[Int]] -> Bool
isWellFormed lxs 
	| length lxs == 1 && null (head lxs) = False
	| otherwise = length (nub [length xs | xs <- lxs]) == 1

-- Returns the dimensions of a n x m matrix as pair (n, m). 
size :: [[Int]] -> (Int, Int)
size lxs 
	| isWellFormed lxs = (length lxs, length $ head lxs) 
	| otherwise = error "Matrix is malformed!" 

-- The function returns element at the given position in Matrix.
getElement :: [[Int]] -> Int -> Int -> Int
getElement matrix i j
	| not $ isWellFormed matrix = error "Matrix is malformed!"
	| not $ isIndexValid matrix i j = error "Index out of bounds!"
	| otherwise = (matrix !! i) !! j

-- Checks if matrix contains element with given indexes.
isIndexValid :: [[Int]] -> Int -> Int -> Bool
isIndexValid matrix i j 
	| (i < 0) || (j < 0) = False
	| i >= fst (size matrix) || j >= snd (size matrix) = False
	| otherwise = True 

-- Returns row-th row of a matrix.
getRow :: [[Int]] -> Int -> [Int]
getRow matrix row 
	| not $ isWellFormed matrix = error "Matrix is malformed!"
	| not $ isIndexValid matrix row 0 = error "Index out of bounds!"
	| otherwise = matrix !! row

-- Returns col-th col of a matrix.
getCol :: [[Int]] -> Int -> [Int]
getCol matrix col
	| not $ isWellFormed matrix = error "Matrix is malformed!"
	| not $ isIndexValid matrix 0 col = error "Index out of bounds!"
	| otherwise = [xs !! col | xs <- matrix]

-- Returns sum of two given matrices.
addMatrices :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices lxs lys 
	| size lxs /= size lys = error "Matrices are not of equal size!"
	| otherwise = 
		[[x + y | (x, idx) <- pairUp xs, (y, idy) <- pairUp ys, idx == idy]
		 | (xs, idxs) <- pairUpList lxs, (ys, idys) <- pairUpList lys, 
		 idxs == idys] 

-- Creates pair of value and index for values in the list.
pairUp :: [Int] -> [(Int, Int)]
pairUp xs = zip xs [1..]

-- Creates pair of list and index for sublists in the list.
pairUpList :: [[Int]] -> [([Int], Int)]
pairUpList lxs = zip lxs [1..]

-- Alternative implementation 1.
addMatrices' :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices' lxs lys
	| size lxs /= size lys = error "Matrices are not of equal size!"
	| otherwise = [addRows xs ys | (xs, ys) <- zip lxs lys]

-- Alternative implementation 2.
addMatrices'' :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices'' = zipWith (zipWith (+))

-- Returns sum of two lists.
addRows :: [Int] -> [Int] -> [Int]
addRows = zipWith (+) 

-- The function transposes matrix.
transpose' :: [[Int]] -> [[Int]]
transpose' matrix
	| not $ isWellFormed matrix = error "Matrix is malformed!"
	| otherwise = [getCol matrix idx | idx <- [0..length (head matrix) - 1]]

-- The function multMatrices multiplies given matrices.
multMatrices :: [[Int]] -> [[Int]] -> [[Int]]
multMatrices lxs lys
	| not $ areMatricesMultipliable lxs lys = error "Incompatible matrix dimensions!"
	| otherwise = [[sum(zipWith (*) (getRow lxs i) (getCol lys j)) | 
	j <- [0..length(head lys) - 1]] | i <- [0..length lxs - 1]]

-- Checks if given matrices have compatible dimensions for multiplying.
areMatricesMultipliable :: [[Int]] -> [[Int]] -> Bool
areMatricesMultipliable lxs lys 
	| not (isWellFormed lxs && isWellFormed lys) = error "Matrix is malformed!"
	| otherwise = length (head lxs) == length lys   

-- TASK02

-- Prints the number of lines, words and characters within the string.
wc :: String -> (Int, Int, Int)
wc xs = (length(lines xs), length(words xs), length xs)

-- Pairs up lines from two inputs and joins them with a tab character.
paste :: [String] -> [String] -> [String]
paste lxs lys  
	| length lxs < length lys = zipWith tabConcat (fill lxs (length lys)) lys
	| length lys < length lxs = zipWith tabConcat lxs (fill lys (length lxs)) 
	| otherwise = zipWith tabConcat lxs lys

-- Concatenates two strings with tab. If one of given strings is null other one is returned
-- with no tab concatenated.
tabConcat :: String -> String -> String
tabConcat xs ys 
	| null ys = xs
	| null xs = ys
	| otherwise = xs ++ "\t" ++ ys 

-- Fills list with empty strings until string length isn't equal to given length. 
fill :: [String] -> Int -> [String]
fill lxs len = if len == 0 then lxs else fill (lxs ++ [""]) (len - 1)

-- The cut utility, taking a delimiter, index and contents as lines, then cuts out
-- a portion of each line.  The parts are separated by the given delimiter and the
-- output part is determined by the given index. 
cut :: String -> Int -> [String] -> [String]
cut del i lxs 
	| length del /= 1 = error "Delimter length should be 1!"
	| otherwise = [splitOn del xs !! (i - 1) | xs <- lxs] 

-- Takes list of String and concatenates each String with new line.
newLines :: [String] -> [String]
newLines lxs = [xs ++ "\n" | xs <- lxs] 

-- Main function takes care of input arguments and other IO operations.
main :: IO ()
main = do
	lxs <- getArgs 
	if head lxs == "wc"
		then do
			print lxs
			file <- readFile (lxs !! 1)
			print (wc file)
		else 
			if head lxs == "cut"
				then do
					print lxs
					file <- readFile (lxs !! 3)
					putStr (concat (newLines $ cut (lxs !! 1) (read (lxs !! 2) :: Int) (lines file)))
				else 
					if head lxs == "paste"
						then do
							file1 <- readFile (lxs !! 1)
							file2 <- readFile (lxs !! 2)
							putStr $ concat (newLines $ paste (lines file1) (lines file2))
						else undefined

-- TASK03

type Node = Int
type Edge = (Node, Node)

-- Calculates number cyclic walks of length k in undirected graph.
cyclicWalks :: [Edge] -> Int -> Int
cyclicWalks xs k 
	| k <= 0 = error "Cycle length must be greater than 1!"
	| otherwise = sum [(adjMatrixPow !! i) !! i | i <- [0..length adjMatrixPow - 1]]
				where adjMatrixPow = powMatrix (genAdjMatrix xs) k

-- Generates adjacency matrix from given list of edges
genAdjMatrix :: [Edge] -> [[Int]]
genAdjMatrix xs = 
	[[v | idx <- [1..n], let v = if checkEdge idx idy xs then 1 else 0] |  idy <- [1..n]]
	where n = findMaxNode xs  

-- Multiplies first matrix with second K times.
multMatricesK :: [[Int]] -> [[Int]] -> Int -> [[Int]] 
multMatricesK lxs lxy k
	| k < 0 = error "Only postive K values allowed!"
	| k == 0 = unitMatrix (length lxs)
	| otherwise = if k == 1 then lxs else multMatricesK (multMatrices lxs lxy) lxy (k - 1)  

unitMatrix :: Int -> [[Int]]
unitMatrix n = [[v | idx <- [1..n], let v = if idx == idy then 1 else 0] | idy <- [1..n]]

-- Delegates calculation of matrix power to multMatricesK function. Passes same matrix twice as
-- function argument. 
powMatrix :: [[Int]] -> Int -> [[Int]]
powMatrix matrix = multMatricesK matrix matrix

-- Checks if given edge is in given list.
checkEdge :: Int -> Int -> [Edge] -> Bool
checkEdge idx idy xs = elem (idx, idy) xs || elem (idy, idx) xs

-- Finds maximum value of node in list of edges. This way size of adjacency matrix is determined.
findMaxNode :: [Edge] -> Node
findMaxNode xs 
	| fst (maximum xs) > fst (maximum rxs) = fst (maximum xs)  
	| otherwise = fst (maximum rxs)
	where rxs = [(b, a) | (a, b) <- xs] 

-- TASK04

type Point = (Double, Double)

-- Creates list of neighbour points and delegates job of calculation to function fun.
minSpeed :: [Point] -> Double 
minSpeed xs = fun 0 (zip xs (tail xs)) 0 

-- Calculates minimum speed recursively.
fun :: Double -> [(Point, Point)] -> Int -> Double
fun currentSpeed xs n = 
	if n < length xs
		then	
			if (currentSpeed + nextSlope (xs !! n)) <= 0
				then -(currentSpeed + nextSlope (xs !! n)) + 
				fun 0 xs (n + 1)
			else fun (currentSpeed + nextSlope (xs !! n)) xs (n + 1)
	else 0

-- Calculates length of slope between given points. Length will be negative is slope between points
-- is negative.
nextSlope :: (Point, Point) -> Double
nextSlope (a, b) = k * sqrt((snd b - snd a)**2 + (fst b - fst a)**2)
	where k = if ((snd b - snd a)/(fst b - fst a)) > 0 then -1 else 1