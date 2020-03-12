module Homework where

import Data.Char

-- TASK01

-- Maps various functions from fs over list xs.
cycleMap :: [a -> b] -> [a] -> [b]
cycleMap _ [] = [];
cycleMap [] _ = [];
cycleMap fs xs =  cycleMap' fs xs
	where
		cycleMap' _ [] = [];
		cycleMap' [] zs = cycleMap' fs zs 
		cycleMap' (z:zs) (y:ys) = z y : cycleMap' zs ys

-- TASK02

-- Reduces list of elements to single element.
reduce :: (a -> b -> a) -> a -> [b] -> a
reduce _ seed [] = seed; 
reduce f seed (x:xs) = reduce f (f seed x) xs

-- Same as reduce. Assumes that given list has at least one element.
reduce1 :: (a -> a -> a) -> [a] -> a
reduce1 _ [] = error "reduce 1 got an empty list"
reduce1 _ [x] = x;
reduce1 f (x:xs) = f x (reduce1 f xs)

-- Returns a list of all the intermediate values with the result at 
-- the end instead of just the last result.
scan :: (a -> b -> a) -> a -> [b] -> [a]
scan _ seed [] = [seed];
scan f seed (x:xs) = seed : scan f (f seed x) xs 

-- Preforms similary to reduce, only does operations from right to
-- left.
rreduce :: (a -> b -> b) -> b -> [a] -> b
rreduce f seed [x] = f x seed
rreduce _ seed [] = seed
rreduce f seed (x:xs) =  f x $ rreduce f seed xs

-- Preforms similary to rreduce but assumes that given list has at
-- least one element.
rreduce1 :: (a -> a -> a) -> [a] -> a
rreduce1 _ [] = error "rreduce1 got an empty list"
rreduce1 _ [x] = x
rreduce1 f (x:xs) = f x $ rreduce1 f xs

-- Preforms similary to scan function but works from right to left.
rscan :: (a -> b -> b) -> b -> [a] -> [b]
rscan _ seed [] = [seed]
rscan f seed xs = rscan' seed $ reverse xs
	where 
		rscan' seed' [] = [seed'];
		rscan' seed' (z:zs) = rscan' (f z seed') zs ++ [seed']




-- TASK03
type Tolerance = Double

-- Approximates sqrt function with newton's method and tolerance
-- given.
newton :: Tolerance -> Double -> Double
newton t x
	| x < 0 = error "can't get sqrt of negative number" 
	| otherwise = newton' 10
	where
		newton' s = if abs (s - expr x s) <  abs t 
			then expr x s 
			else newton' (expr x s) 
		expr a b = (b + a/b)/2

dx :: Double
dx = 0.00001

-- Approximates derivation of function f in x.
deriv :: (Double -> Double) -> Double -> Double
deriv f x = (f (x + dx) - f x)/dx 

-- TASK04

type Operators = [(Char, Int -> Int -> Int)]
basic :: Operators
basic    = [ ('+', (+))
		   , ('-', (-)) ]
standard :: Operators
standard = [ ('+', (+))
		   , ('-', (-)) 
           , ('*', (*))
           , ('/', div)
           , ('^', (^)) ]

-- Returns operator corresponding to given character. If operator
-- isn't in list of operators returns error.
getOperator :: Operators -> Char -> Int -> Int -> Int
getOperator ops ch 
	| null filtered = error $ "Invalid simbol " ++ [ch]
	| otherwise = snd $ head filtered
	where 
		filtered = filter cond ops
		cond (x, _) = ch == x


-- Returns result of reverse polish notation string. It works
-- only with one digit positive integers. Delegates work to rpnEngine
-- function.
rpnCalc :: String -> Operators -> Int
rpnCalc [] _ = 0;
rpnCalc xs ops = rpnEngine [] xs ops

-- Returns result of reverse polish notation string. Provide empty
-- list as stack for this function.
rpnEngine :: [Int] -> String -> Operators -> Int
rpnEngine [x] [] _ = x;
rpnEngine stack (x:xs) ops
	| isDigit x = rpnEngine (digitToInt x:stack) xs ops
	| length stack == 1 = error "Invalid RPN expression"
	| otherwise = rpnEngine (evaluate x ops stack) xs ops

-- Evaluates result of operation given as character if it is 
-- found in list of operators. Operator is applied to two first
-- arguments of given integer list(stack).
evaluate :: Char -> Operators -> [Int] -> [Int]
evaluate op ops (x:y:xs) = f y x : xs
	where 
		f = getOperator ops op 

-- TASK05

-- Returns minimal number of jumps required for frogs to go from
-- first to third lilly pad.
frogJumps :: Int -> Integer
frogJumps 0 = 0
frogJumps n 
	| n > 0 = 2 + 3 * frogJumps (n - 1)
	| otherwise = error "provide positive integer as number of frogs"