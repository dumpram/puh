module Exercises where

-- | Exercise1

takeThree :: [a] -> [a]
takeThree = take 3

dropThree :: [a] -> [a]
dropThree = drop 3

hundredTimes :: a -> [a]
hundredTimes = replicate 100

index :: [a] -> [(Int, a)]
index = zip [0..] 

index' :: [a] -> [(a, Int)]
index' = (`zip` [0..])

divider :: Int -> String
divider = (`replicate` '=')  


-- | Exercise2

applyOnLast :: (a -> a -> a) -> [a] -> [a] -> a
applyOnLast f xs ys = f (last xs) (last ys)

addThree :: Num a => a -> a -> a -> a
addThree x y z = x + y + z

lastTwoPlus100 :: [Integer] -> [Integer] -> Integer
lastTwoPlus100 = applyOnLast (addThree 100)

applyManyTimes ::  Int -> (a -> a) -> a -> a
applyManyTimes n f x
	| n <= 0 = x
	| otherwise = applyManyTimes (n - 1) f (f x)

applyTwice :: (a -> a) -> a -> a
applyTwice = applyManyTimes 2

-- | Exercise3

listifylist :: [a] -> [[a]]
listifylist = map (:[])

cutoff :: Int -> [Int] -> [Int]
cutoff n = map fun
	where fun x = if x > n then n else x

cutoff' :: Int -> [Int] -> [Int]
cutoff' n = map (min n)

-- | Exercise4

sumEvenSquares :: [Int] -> Int
sumEvenSquares xs = sum $ map sqr (filter even xs)
	where sqr a = a * a

freq :: Eq a => a -> [a] -> Int
freq x  = length . filter (==x)

freqFilter :: Eq a => Int -> [a] -> [a]
freqFilter n xs = filter freqC xs
	where freqC x = freq x xs >= n

-- | Exercise5

withinInterval :: Integer -> Integer -> [Integer] -> [Integer]
withinInterval n m = filter (\x -> x > n && x < m)  

sndColumn :: [[a]] -> [a]
sndColumn = map (\(_:y:_) -> y)

canonicalizePairs :: Ord a => [(a, a)] -> [(a, a)]
canonicalizePairs = map (\(x,y) -> (min x y, max x y)) . filter (uncurry (/=))