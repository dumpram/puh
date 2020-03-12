module Exercises where


import Data.Char
import Data.List
import Data.List.Split
import Data.Function

-- | Exercise1

sumEven :: [Integer] -> Integer
sumEven = sum . takeEven
	where
		takeEven [] = []
		takeEven [x] = [x]
		takeEven (x:_:xs) = x:takeEven xs


--sumEven' :: [Integer] -> Integer
--sumEven' = sum . map snd . filter (even . fst) . zip [0..]


filterWords :: [String] -> String -> String
filterWords ws = concat . filter (`notElem` ws) . words  

initials3 :: String -> (String -> Bool) -> String -> String
initials3 d p = concatMap (++d) . group . map (toUpper . head) . filter p . words

initials :: String -> String
initials = initials3 "" (const True)

-- | Exercise2

maxDiff :: [Int] -> Int
maxDiff xs = maximum $ map abs $ zipWith (-) xs (tail xs)

minDiff :: [Int] -> Int
minDiff xs = minimum $ map abs $ zipWith (-) xs (tail xs)

maxMinDiff :: [Int] -> (Int, Int)
maxMinDiff xs = (maxDiff xs, minDiff xs)

studentsPassed :: [(String, Double)] -> [String]
studentsPassed xs = map fst $ filter ((>=0.5 * maxi) . snd) xs
	where 
		maxi = maximum $ map snd xs   

-- | Exercise3

isTitleCased :: String -> Bool
isTitleCased = all isUpper . map head . words

sortPairs :: [(Integer, Integer)] -> [(Integer, Integer)]
sortPairs = sortBy (compare `on` snd)

filename :: String -> String 
filename = last . splitOn "/"

maxElemIndices :: Ord a => [a] -> [Int]
maxElemIndices [] = error "empty list"
maxElemIndices xs = map fst . filter (\(_,y) -> y == maximum xs) $ zip [0..] xs

maxElemIndices' :: Ord a => [a] -> [Int]
maxElemIndices' [] = error "empty list"
maxElemIndices' xs = findIndices (\x -> x == maximum xs) xs 

uncurriedMax :: Ord a => (a, a) -> a
uncurriedMax = uncurry max

-- | Exercise4

elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr (\y acc -> acc || (x == y)) False 

reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++ [x]) []

nubRuns :: Eq a => [a] -> [a]
nubRuns = foldr f []
	where 
		f x [] = [x]
		f x ys@(y:_) = if x /= y then x:ys else ys 

-- | Exercise5

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []
--reverse'' = foldl (\acc x -> x:acc) []

-- | Commented because of warnings in sublime.
-- sumEven'' :: [Integer] -> Integer
-- sumEven'' = snd . foldl f (0, 0)
-- 	where 
-- 		f (idx, acc) x
-- 			| even idx = (idx + 1, acc + x)
-- 			| otherwise = (idx + 1, acc) 

maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip [] = error "empty list"
maxUnzip xs = foldl f (head xs) xs 
	where
		f (x1, x2) (acc1, acc2) = (max x1 acc1, max x2 acc2)
		
