module Main where

import CSVUtils
import System.Environment

-- TASK01

-- Note: Some functions which are used to calculate other functions 
-- etc. mean'' should not be public. It would be better to use where
-- statement but for educational reasons.

type Probability = Double
type DiscreteRandVar = [(Int, Probability)]

x :: DiscreteRandVar
x = [(1, 0.2), (2, 0.4), (3, 0.1), (4, 0.2), (5, 0.05), (6, 0.05)]

-- Returns mean value of given discrete random variable. Implementation
-- is recursive without accumulator.
mean :: DiscreteRandVar -> Double
mean [] = 0
mean ((xi, p):xs) = fromIntegral xi * p + mean xs 

-- Returns mean value of given discrete random variable. Implementation
-- is recursive with accumulator.
mean' :: DiscreteRandVar -> Double
mean' xs = mean'' xs 0 

-- mean' delegates job of calculating mean value of discrete random variable
-- to this function. 
mean'' :: DiscreteRandVar -> Double -> Double
mean'' [] acc = acc;
mean'' ((xi, p):xs) acc = mean'' xs (acc + fromIntegral xi * p)

-- Returns variance of given discrete random variable. Implementation
-- is recursive without  accumlator.
variance :: DiscreteRandVar -> Double
variance xs = varianceWrap xs (mean xs)

-- Returns variance of given discrete random variable. Implementation
-- is recursive with  accumlator. 
variance' :: DiscreteRandVar -> Double
variance' xs = varianceWrap' xs (mean xs) 0

-- variance' delegates job of calculating mean value of discrete random variable
-- to this function.
varianceWrap :: DiscreteRandVar -> Double -> Double
varianceWrap [] _ = 0;
varianceWrap ((xi, p):xs) m = (fromIntegral xi - m)**2 * p + varianceWrap xs m

-- variance' delegates job of calculating variance of discrete random variable
-- to this function.
varianceWrap' :: DiscreteRandVar -> Double -> Double -> Double
varianceWrap' [] _ acc = acc 
varianceWrap' ((xi, p):xs) m acc = varianceWrap' xs m acc + (fromIntegral xi - m)**2 * p 

-- Returns all values of which probability is greater or equal to given value.  
probabilityFilter :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter _ [] = [];
probabilityFilter r ((xi, p):xs) = if p >= r then xi:probabilityFilter r xs else probabilityFilter r xs

-- Accumulator version of probability filter.
probabilityFilter' :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter' r xs = probabilityFilterWrap r xs []

-- probabilityFilter' delegates job of filtering to this function.
probabilityFilterWrap :: Probability -> DiscreteRandVar -> [Int] -> [Int]
probabilityFilterWrap _ [] acc = reverse acc;
probabilityFilterWrap r ((xi, p):xs) acc = probabilityFilterWrap r xs (if p >= r then xi:acc else acc)

--TASK02

-- CSVUtils.hs

-- TASK03

type Set a = [a]

-- Returns set from given list. 
setify :: Eq a => [a] -> Set a
setify [] = []
setify (y:ys) = y:setify (removeDuplicatesOf y ys)  
	where 
		removeDuplicatesOf u (v:vs) = if u /= v then v:removeDuplicatesOf u vs else removeDuplicatesOf u vs 
		removeDuplicatesOf _ [] = [];

-- Checks if set contatins given element.
contains :: Eq a => Set a -> a -> Bool
contains [] _ = False
contains (y:ys) z = (y == z) || contains ys z

-- Inserts element in set.
-- Note: Poor complexity due to concatenating element on the end of set.
-- It is made this way because of test examples.
insert' :: Eq a => Set a -> a -> Set a
insert' [] y = [y]
insert' ys z = if contains ys z then ys else ys ++ [z] 

-- Returns set which is union of two given sets.
union' :: Eq a => Set a -> Set a -> Set a
union' ys zs = setify (ys ++ zs)

-- Returns set which is intersection of two given sets.
intersection :: Eq a => Set a -> Set a -> Set a
intersection [] _ = []
intersection _ [] = []
intersection (y:ys) zs = if contains zs y then y:intersection ys zs else intersection ys zs 

-- Returns set which is difference of given sets.
difference :: Eq a => Set a -> Set a -> Set a
difference [] _ = []
difference ys [] = ys
difference (y:ys) zs = if contains zs y then difference ys zs else y:difference ys zs 
  
-- TASK04
-- Note: I tried to generalize code so it is possible two have more than one
-- decision and more than 3 features in feature vector.


type Decision = String
caseA :: [String]
caseA = ["Raining", "Yes", "Workday"]

caseB :: [String]
caseB = ["Sunny", "No", "Workday"]

type FeatureVector = [String]

-- Returns decision for given feature vector base on dataset. 
-- Finds maximum probability from list of tuples which contain
-- probability and decision.
nbDecide :: CSV -> [String] -> Decision
nbDecide csv fv = snd (maximum (decisions csvWithNoHead fv decs))
	where 
		csvWithNoHead = tail csv 
		decs = setify(colFields (length(head csv) - 1) csvWithNoHead)

-- Returns list of decisions for list of given feature vectors based
-- on dataset.
nbDecideAll :: CSV -> [[String]] -> [Decision]
nbDecideAll _ [] = []
nbDecideAll csv (fv:fvs) = nbDecide csv fv : nbDecideAll csv fvs

-- Takes care of input and output actions.
main :: IO ()
main = do
	args <- getArgs
	csv <- readCSV ";" (head args)
	print (nbDecide csv ["Raining", "Yes", "Workday"]) 

-- Returns list of tuples which contain decision and its probability.
-- It recursively iterates over set of decisions in dataset.
decisions :: CSV -> FeatureVector -> Set Decision -> [(Probability, Decision)]
decisions _ _ [] = []
decisions csv fv (dec:decs) = decisionProbability csv fv dec 1 0 : decisions csv fv decs

-- Returns probability of given decision based on given dataset and feature vector.
decisionProbability :: CSV -> FeatureVector -> Decision -> Double -> Int -> (Probability, Decision)
decisionProbability csv [] dec acc _ = (acc * fromIntegral countDecision / fromIntegral (length lastColumn) , dec)
	where 
		countDecision = length (filter cond lastColumn)
		cond el = dec == el
		lastColumn = colFields (length (head csv) - 1) csv

decisionProbability csv (f:fv) dec acc idx = 
	decisionProbability csv fv dec 
	(acc * featureProbabilty f dec (zip (colFields idx csv) (colFields (length(head csv) - 1) csv))) (idx + 1)

-- Returns probability of feature for some decision.
featureProbabilty :: String -> String -> [(String, String)] -> Double
featureProbabilty feature decision zippedList =  fromIntegral countFeature / fromIntegral countDecision
	where 
		countFeature = length (filter cond zippedList)
		countDecision = length (filter cond2 zippedList)
		cond (a,b) = a == feature && b == decision 
		cond2 (_,b) = b == decision

