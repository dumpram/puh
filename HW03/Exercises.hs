module Exercises where

import Data.List

import Data.Char
-- | Exercise01

headHunter :: [[a]] -> a
headHunter ((x:_):_) = x
headHunter (_:(x:_):_) = x;
headHunter (_:_:(x:_):_) = x;
headHunter _ = error "No heads to take!"

firstColumn :: [[a]] -> [a]
firstColumn m = [x | (x:_) <- m]

firstColumn' :: [[Int]] -> [Int]
firstColumn' = map head

shoutOutLoud' :: String -> String
shoutOutLoud' s = unwords (map fun (words s))
	where 
		fun [] = []
		fun x = replicate 3 (head x) ++ tail x
shoutOutLoud :: String -> String
shoutOutLoud xs = unwords [replicate 3 x ++ rest | (x:rest) <- words xs]

-- | Exercise02

pad :: String -> String -> (String, String)
pad xs ys = (upper longer, upper shorter)
	where 
		longer = if length xs > length ys then xs else ys
		shorter = if length xs > length ys then padding ys else padding xs 
		padding zs = zs ++ replicate (abs (length xs - length ys)) ' '
		upper  (w:ws) = toUpper w : ws

median :: (Integral a, Fractional b) => [a] -> b
median [] = error "median: Empty list"
median xs 
   | odd l     = realToFrac $ ys !! h
   | otherwise = realToFrac (ys !! h + ys !! (h-1)) / 2
	where 
		l  = length xs         
		h  = l `div` 2
		ys = sort xs


quartiles :: [Int] -> (Double, Double, Double)
quartiles xs = (first, second, third)
 	where 
 		splitInHalf ys = splitAt (length ys `quot` 2) ys
 		second = median xs
 		first = median (fst (splitInHalf (sort xs)))
 		third = median (snd (splitInHalf (sort (tail xs)))) 




-- | Exercise03

quartiles' :: [Int] -> (Double, Double, Double)
quartiles' xs =
 	let	splitInHalf ys = splitAt (length ys `quot` 2) ys
 		second = median xs
 		first = median (fst (splitInHalf (sort xs)))
 		third = median (snd (splitInHalf (sort (tail xs))))
 	in (first, second, third) 


-- | Exercise04

caseFunction :: Show a => (Int, Int) -> [a] -> String
caseFunction (a, b) xs = "The pair " ++ case (a,b) of
	(1, 1) -> "contains two ones " ++ rest xs
	(_, 1) -> "contains one one " ++ rest xs
	(1, _) -> "contains one one " ++ rest xs
	(_, _) -> "does not contain a single one " ++ rest xs
	where 
		rest (_:y:_) = "and the second element of list is " ++ show y 
		rest _ = "and there is no second element in list"