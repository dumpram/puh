module Main where

import Matrix
type Node = Int
type Edge = (Node, Node)

cyclicWalks :: [Edge] -> Int -> Int
cyclicWalks xs k 
	| k <= 0 = error "Cycle length must be greater than 1!"
	| otherwise = sum [(adjMatrixPow !! i) !! i | i <- [0..length adjMatrixPow - 1]]
				where adjMatrixPow = powMatrix' (genAdjMatrix xs) k

genAdjMatrix :: [Edge] -> [[Int]]
genAdjMatrix xs = 
	[[v | idx <- [1..n], let v = if checkEdge idx idy xs then 1 else 0] |  idy <- [1..n]]
	where n = findMaxNode xs  

powMatrix :: [[Int]] -> [[Int]] -> Int -> [[Int]] 
powMatrix lxs lxy k
	| k < 0 = error "Only postive power values allowed!"
	| k == 0 = unitMatrix (length lxs)
	| otherwise = if k == 1 then lxs else powMatrix (multMatrices lxs lxy) lxy (k - 1)  

unitMatrix :: Int -> [[Int]]
unitMatrix n = [[v | idx <- [1..n], let v = if idx == idy then 1 else 0] | idy <- [1..n]]

powMatrix' :: [[Int]] -> Int -> [[Int]]
powMatrix' matrix = powMatrix matrix matrix

checkEdge :: Int -> Int -> [Edge] -> Bool
checkEdge idx idy xs = elem (idx, idy) xs || elem (idy, idx) xs

findMaxNode :: [Edge] -> Node
findMaxNode xs 
	| fst (maximum xs) > fst (maximum rxs) = fst (maximum xs)  
	| otherwise = fst (maximum rxs)
	where rxs = [(b, a) | (a, b) <- xs] 

main :: IO ()
main = putStrLn "Hello!"