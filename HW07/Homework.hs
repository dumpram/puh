module Homework where

import Data.List
import Data.Ord

-- takeWhile' :: (a -> Bool) -> [a] -> [a]
-- -- takeWhile' p xs = takeHelp' xs []
-- -- 	where 
-- -- 		takeHelp' [] acc = acc
-- -- 		takeHelp' y:ys acc = if p y then takeHelp' ys y:acc else ys

-- takeWhile' p = snd . foldl (\acc x -> if p x && fst acc then (True, snd acc ++ [x]) else (False, snd acc)) (True, [])


-- takeWhile'' :: (a -> Bool) -> [a] -> [a]
-- takeWhile'' p = snd . foldl f (True, [])
-- 	where
-- 		f (False, acc) _ = (False, acc)
-- 		f (True, acc) x = if p x then (True, acc ++ [x]) else (False, acc)  


-- dropWhile' :: (a -> Bool) -> [a] -> [a]
-- dropWhile' p = snd . foldr f (True, []) 
-- 	where
-- 		f _ (False, acc) = (False, acc)
-- 		f x (True, acc) = if not $ p x then (True, x:acc) else (False, acc)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = takeHelp True
	where
		takeHelp _ [] = []
		takeHelp stillTaking (x:xs) = if p x && stillTaking then x:takeHelp True xs else takeHelp False []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p = dropHelp True
	where
		dropHelp _ [] = []
		dropHelp stillDropping (x:xs) = if p x && stillDropping then dropHelp True xs else x:dropHelp False xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = [];
zipWith' _ _ [] = [];
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 


efficientSortBy :: Ord b => (a -> b) -> [a] -> [a]
efficientSortBy f xs = map snd $ sortBy (comparing fst) (zip (map f xs) xs)




sortByExample :: Ord a => [a] -> [a]
sortByExample = sortBy order 
	where 
		order x y
			| x < y = GT
			| x > y = LT
			| x == y = EQ
