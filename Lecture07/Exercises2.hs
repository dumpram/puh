module Exercises2 where


import Data.Char
import Data.List
import Data.List.Split
import Data.Function

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


--takeEven' :: [a] -> [a]
--takeEven' = map snd . filter (even . fst) . zip [0..]


maxDiff :: [Int] -> Int
maxDiff xs = maximum $ map abs $ zipWith (-) xs (tail xs)

minDiff :: [Int] -> Int
minDiff xs = minimum $ map abs $ zipWith (-) xs (tail xs)

maxMinDiff :: [Int] -> (Int, Int)
maxMinDiff xs = (maxDiff xs, minDiff xs)


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

elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr (\y acc -> acc || (x == y)) False 

reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++ [x]) []

nubRuns :: Eq a => [a] -> [a]
nubRuns = foldr f []
	where 
		f x [] = [x]
		f x ys@(y:_) = if x /= y then x:ys else ys 

