module Proba where

-- Exercise01

product' :: Num a => [a] -> a
product' [x] = x
product' (x:xs) = x * product xs


headsOf :: [[a]] -> [a]
headsOf [] = []; 
headsOf ([]:lxs) = headsOf lxs
headsOf (xs:lxs) = head xs : headsOf lxs 

-- Exercise02

modMult :: Int -> Int -> [Int] -> [Int]
modMult _ _ [] = [] 
modMult n m (x:xs) = x * (n `mod` m) : modMult n m xs


addPredecessor :: Num a => [a] -> [a] 
addPredecessor [] = []
addPredecessor (x:xs) = x : add x xs 
	where 
	add _ [] = [] 
	add p (y:ys) = (p+y) : add y ys


-- Exercise03

equalTriplets :: Eq a => [(a,a,a)] -> [(a,a,a)]
equalTriplets [] = []
equalTriplets ((a,b,c):xs) = if a == b && b == c && c == a then (a,b,c) : equalTriplets xs else equalTriplets xs


replicate' :: Int -> a -> [a]
replicate' 0 a = [a]
replicate' n a = a : replicate (n - 1) a

-- Exercise04

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (x:xs) = if n /= 0 then drop' (n-1) xs else x:drop' 0 xs

drop'' :: Int -> [a] -> [a]
drop'' n xs 
	| n < 0 = drop' (-n) (reverse xs)
	| otherwise = drop' n xs


takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo _ _ [] = [];
takeFromTo 0 0 _ = [];
takeFromTo n1 n2 (x:xs) =  if n1 == 0 && n2 >= 0 then x:takeFromTo 0 (n2-1) xs else takeFromTo (n1 - 1) (n2 - 1) xs

-- Exercise05

eachThird :: [a] -> [a]
eachThird [] = [];
eachThird [a] = [a]
eachThird [a, _] = [a];
eachThird (a:_:_:rest) = a:eachThird rest; 

crossZip :: [a] -> [a] -> [(a,a)]
crossZip [] _ = []
crossZip _ [] = []
crossZip [a][b] = [(a, b)]
crossZip [a,b] [c,d] = [(a,d),(b,c)] 
crossZip (a:b:xs) (c:d:ys)
	= (a,d):(b,c):crossZip xs ys


-- Exercise06
length' :: [a] -> Int
length' xs = len xs 0
	where 
		len [] size = size
		len (_:ys) size = len ys (size + 1)

maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip [] = error "empty list"
maxUnzip ((x,y):zs) = maxPair x y zs 
	where
		maxPair max1 max2 [] = (max1, max2)
		maxPair max1 max2 ((a,b):ys) = maxPair (max max1 a) (max max2 b) ys  