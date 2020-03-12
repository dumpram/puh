module MidtermExam where


import Data.List
-- import Data.Function

-- finalRanking :: [(String, Int)] -> [String]
-- finalRanking = sort . map fst . sortBy (compare `on` snd)


ranking :: [(String, Int)] -> [String]
ranking = map fst . sortBy f 
	where f (name1, score1) (name2, score2)
		| score1 < score2 = GT
		| score1 > score2 = LT
		| otherwise = compare name1 name2


zipN :: [[a]] -> [[a]]
zipN xs = reverse $ engine (map (take len) xs) []
	where 
		len = minimum (map length xs)
		engine ys acc = if not (cond ys) then engine (map tail ys) (map head ys:acc) else acc
		cond zs = maximum (map length zs) == 0 

zipN' :: [[a]] -> [[a]]
zipN' xss = help xss []
	where 
		help yss acc = help (map (drop 1) yss) (map head yss):acc

mapWhile :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapWhile p f = map mapHelp
	where
		mapHelp x = if p x then mapHelp $ f x else x  


