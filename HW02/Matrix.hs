module Matrix where

import Data.List

-- The function checks whether the matrix has all rows of equal length.
isWellFormed :: [[Int]] -> Bool
isWellFormed lxs 
	| length lxs == 1 && null (head lxs) = False
	| otherwise = length (nub [length xs | xs <- lxs]) == 1

-- The function returns the dimensions of a n x m matrix as pair
-- (n, m). 
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

-- The function checks if matrix contains element with given indexes.
isIndexValid :: [[Int]] -> Int -> Int -> Bool
isIndexValid matrix i j 
	| (i < 0) || (j < 0) = False
	| i >= fst (size matrix) || j >= snd (size matrix) = False
	| otherwise = True 

-- The function returns row-th row of a matrix.
getRow :: [[Int]] -> Int -> [Int]
getRow matrix row 
	| not $ isWellFormed matrix = error "Matrix is malformed!"
	| not $ isIndexValid matrix row 0 = error "Index out of bounds!"
	| otherwise = matrix !! row

-- The function returns col-th col of a matrix.
getCol :: [[Int]] -> Int -> [Int]
getCol matrix col
	| not $ isWellFormed matrix = error "Matrix is malformed!"
	| not $ isIndexValid matrix 0 col = error "Index out of bounds!"
	| otherwise = [xs !! col | xs <- matrix]

-- The function returns sum of two given matrices.
addMatrices :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices lxs lys 
	| size lxs /= size lys = error "Matrices are not of equal size!"
	| otherwise = 
		[[x + y | (x, idx) <- pairUp xs, (y, idy) <- pairUp ys, idx == idy]
		 | (xs, idxs) <- pairUpList lxs, (ys, idys) <- pairUpList lys, 
		 idxs == idys] 

-- The function creates pair of value and indexf for values in the list.
pairUp :: [Int] -> [(Int, Int)]
pairUp xs = zip xs [1..]

pairUpList :: [[Int]] -> [([Int], Int)]
pairUpList lxs = zip lxs [1..]

addMatrices' :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices' lxs lys
	| size lxs /= size lys = error "Matrices are not of equal size!"
	| otherwise = [addRows xs ys | (xs, ys) <- zip lxs lys]

addMatrices'' :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices'' = zipWith (zipWith (+))


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