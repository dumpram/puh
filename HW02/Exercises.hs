module Exercises where

import Data.List
import Data.Char

-- Exercise 1

threeList :: [a] -> [a]
threeList list 	
	| length list >= 6 = drop 3 (reverse (drop 3 (reverse list)))
	| otherwise = error "List is too short!"

initials :: String -> String -> String
initials s1 s2 
	| not (null s1) && not (null s2) = head s1 : "." ++ head s2 : "."
	| otherwise = error "Empty string provided!"

concatLong :: [a] -> [a] -> [a]
concatLong s1 s2 
	| length s1 > length s2 = s1 ++ s2
	| otherwise = s2 ++ s1

safeHead :: [a] -> [a]
safeHead list 	
	| null list = list
	| otherwise = [head list]

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates list = length list > length (nub list)

-- Exercise 2

doublesFromTo :: Int -> Int -> [Int]
doublesFromTo a b 
   | b < a     = doublesFromTo b a
   | otherwise = [x*2 | x <- [a..b]]

ceasarCode :: Int -> String -> String
ceasarCode n xs = [toLower (succn n c) | c <- xs, isLetter (toLower(succn n c))]

succn :: Int -> Char -> Char
succn n c = if n <= 0 then c else succn (n - 1) (succ c)


-- Exercise 3

letterCount :: String -> Int 
letterCount xs = length [x | x <- words xs, length x > 3]

isPalindrome :: String -> Bool
isPalindrome xs = ys == reverse ys
	where ys = [toLower x | x <- xs, x /= ' ']

flipp :: [[a]] -> [a]
flipp xss = concat $ reverse [reverse xs | xs <- xss] 

-- Exercise 4

inCircle :: Double -> Double -> Double -> Double -> [(Double, Double)]
inCircle r x y res = [(a, b) | a <- [-(10 + res)..10], b <-[-(10 + res)..10], (a-x)*(a-x) + (b-y)*(b-y) < r*r]

steps :: [a] -> [(a,a)]
steps xs = zip xs (tail xs)

-- Exercise 5

indices :: Eq a => a -> [a] -> [Int]
indices x xs = [idx | idx <- [0..length xs - 1], (xs !! idx) == x]

showLineNumbers :: String -> String
showLineNumbers xs = concat [show idx ++ " " ++ x ++ "\n" | (x, idx) <- zip (lines xs) [1..length (lines xs)]]

haveAlignment :: String -> String -> Bool
haveAlignment xs ys = or (zipWith (==) xs ys)

common :: String -> String -> String 
common xs ys = [a | (a, b) <- zip xs ys, a == b]

main :: IO ()  
main = undefined