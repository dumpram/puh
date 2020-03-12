module Exercises where

import Data.Function
import Data.List

data Date = Date Int Int Int deriving Show

showDate :: Date -> String
showDate (Date yy dd mm) = show dd ++ "." ++ show mm ++ "."  ++ show yy

data Point = Point Double Double 
   deriving Show
data Shape2 = Circle2 Point Double | Rectangle2 Point Point 
   deriving Show

translate :: Point -> Shape2 -> Shape2
translate (Point x1 y1) (Circle2 (Point x2 y2) r) = Circle2 (Point (x1+x2) (y1+y2)) r
translate (Point x1 y1) (Rectangle2 (Point x2 y2) (Point x3 y3)) = Rectangle2 (Point (x2+x1) (y2+y1)) (Point (x3+x1) (y3+y1))


inShape :: Shape2 -> Point -> Bool
inShape (Circle2 (Point xs ys) r) (Point x y) = (x - xs) * (x - xs) + (y - ys) * (y - ys) <= r * r
inShape (Rectangle2 (Point x1 y1) (Point x2 y2)) (Point x y) = 
  x <= max x1 x2 && x >= min x1 x2 && y <= max y1 y2 && y >= min y1 y2 

inShapes :: [Shape2] -> Point -> Bool
inShapes xs p = all (`inShape` p) xs


data Vehicle = Car String Double | Truck String Double | Motorcycle String Double | Bicycle String

horsepower :: Vehicle -> Double
horsepower (Bicycle _) = 0.2
horsepower (Car _ hp) = hp
horsepower (Truck _ hp) = hp
horsepower (Motorcycle _ hp) = hp

totalHorsepower :: [Vehicle] -> Double
totalHorsepower = sum . map horsepower




data Student = Student
  { firstName  :: String
  , lastName   :: String
  , studentId  :: String
  , level      :: Level
  , avgGrade   :: Double } deriving Show

data Level    = Bachelor | Master | PhD deriving (Show,Eq)

studentGrade :: Student -> Double
studentGrade (Student _ _ _ _ g) = g


studentLevel :: Student -> Level
studentLevel (Student _ _ _ l _) = l

improveStudent :: Student -> Student
improveStudent (Student fn ln i l g) = if g + 1 <= 5 then Student fn ln i l (g + 1) else Student fn ln i l g 


avgGradePerLevels :: [Student] -> (Double, Double, Double)
avgGradePerLevels xs = (mean $ filter ((==Bachelor) . studentLevel) xs, mean $ filter ((==Master) . studentLevel) xs, mean $ filter ((==PhD) . studentLevel) xs)
	where mean zs = sum (map studentGrade zs) / fromIntegral (length zs)


rankedStudents :: Level -> [Student] -> [String] 
rankedStudents l = reverse . map studentId . sortBy (compare `on` avgGrade) . filter (\x -> level x == l) 

-- instance Eq -- posli risiti

-- addStudent :: Student -> [Student] -> [Student]
-- addStudent x xs = if x `elem` xs then error "Student exists in the list" else x:xs

data MyTriplet a b c = MyTriplet (a, b, c)

toTriplet :: MyTriplet a b c -> (a, b, c)
toTriplet (MyTriplet (a, b, c)) = (a, b, c)


data Employee = Employee
   { name   :: String
  , salary :: Maybe Double } deriving Show


showSalary :: Employee -> String 
showSalary e = case salary e of
    Nothing -> "unknown"
    Just n  -> show n ++ " kn"

eSalary :: Employee -> Maybe Double
eSalary (Employee _ s) = s

extract :: Maybe Double -> Double
extract (Just n) = n
extract _ = 0.0

totalSalaries :: [Employee] -> Double
totalSalaries = sum . map (extract . salary) 


addStudent2 :: Student -> [Student] -> Maybe [Student]
addStudent2 = undefined

-- addStudent3 :: Student -> [Student] -> Either [Student]
-- addStudent3 = undefined

