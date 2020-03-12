module Exercises where 

import Data.List
import Data.Maybe (isJust)

data Sex = Male | Female deriving (Show,Read,Ord,Eq)


data Person = Person {
   idNumber :: String,
   forename :: String,
   surname  :: String,
   sex      :: Sex,
   age      :: Int,
   partner  :: Maybe Person,
   children :: [Person] } deriving (Read,Ord,Eq)


partnersForename :: Person -> Maybe String
partnersForename p = case partner p of
	Just a -> Just $ forename a
	Nothing -> Nothing


partnersForename2 :: Person -> Maybe String
partnersForename2 = fmap forename . partner



pairsChildren :: Person -> [Person]
pairsChildren p = nub $ children p ++ case partner p of
	Nothing -> []
	Just r -> children r

pairsChildren2 :: Person -> [String]
pairsChildren2 p = nub $ map forename $ children p ++ case partner p of
	Nothing -> []
	Just r -> children r


partnersMother :: Person -> Maybe Person
partnersMother = undefined



data Person2 = Person2 {
   personId2 :: String,
   forename2 :: String,
   surname2  :: String,
   sex2      :: Sex,   --- data Sex = Male | Female deriving (Show,Read,Eq,Ord)
   mother2   :: Maybe Person2,
   father2   :: Maybe Person2,
   partner2  :: Maybe Person2,
   children2 :: [Person2] } deriving (Show,Read,Eq,Ord)

john :: Person2
john = Person2 "123" "John" "Doe" Male Nothing Nothing (Just jane) []
jane :: Person2
jane = Person2 "623" "Jane" "Fox" Female (Just ann) Nothing (Just john) []
ann :: Person2
ann  = Person2 "343" "Ann"  "Doe" Female Nothing Nothing Nothing [jane]

parentCheck :: Person2 -> Bool
parentCheck p = checkMother && checkFather
	where
		checkMother = p `elem` case mother2 p of
			Nothing -> []
			Just r -> children2 r
		checkFather = p `elem` case father2 p of
			Nothing -> []
			Just r -> children2 r


-- sister :: Person2 -> Maybe Person2
-- sister p = filter (case mother2 p of
-- 	Nothing -> []
-- 	Just r -> children 2 r

sister :: Person2 -> Maybe Person2
sister p
   | parentCheck p = error "Can't conclude because person's data is not consistent"
   | otherwise = extractSister $ filter (\x -> sex2 x == Female) siblings  
   where
      siblings = extractChildren (mother2 p) ++ extractChildren (father2 p)


-- A.K.A. safeHead
extractSister :: [Person2] -> Maybe Person2
extractSister [] = Nothing
extractSister (x:_) = Just x

extractChildren :: Maybe Person2 -> [Person2]
extractChildren p = case p of 
   Nothing -> []
   Just r -> children2 r

concatMaybePersons :: [Maybe Person2] -> [Maybe Person2] -> [Maybe Person2]
concatMaybePersons xs ys = filter isJust xs ++ filter isJust ys

descendant :: Person2 -> [Person2]
descendant p = descendant2 (Just p)


descendant2 :: Maybe Person2 -> [Person2]
descendant2 Nothing = []
descendant2 (Just p) = descendant2 (mother2 p) ++ descendant2 (father2 p)

data MyList a = Empty | Cons a (MyList a) deriving (Show,Read,Ord)

listHead :: MyList a -> Maybe a
listHead Empty = Nothing
listHead (x `Cons` _) = Just x 

listMap :: (a -> b) -> MyList a -> MyList b
listMap _ Empty = Empty
listMap f (x `Cons` xs) = f x `Cons` listMap f xs

data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show)

type IntTree = Tree Int 

treeMax :: Ord a => Tree a -> a
treeMax = maximum . treeToList

treeToList :: Tree a -> [a]
treeToList Null = []
treeToList (Node x left right) = treeToList left ++ [x] ++ treeToList right

levelCut :: Int -> Tree a -> Tree a
levelCut _ Null = error "Too large level of tree!"
levelCut l (Node x right left) 
   | l > 0 = Node x (levelCut (l-1) left) (levelCut (l-1) right)
   | l == 0 = Node x Null Null
   | otherwise = error "Wrong level cut parameter!"


treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Null = Node x Null Null
treeInsert x t@(Node y l r) 
  | x < y     = Node y (treeInsert x l) r
  | x > y     = Node y l (treeInsert x r)
  | otherwise = t

listToTree :: Ord a => [a] -> Tree a
listToTree = foldr treeInsert Null 


sortAndNub :: Ord a => [a] -> [a]
sortAndNub = treeToList . listToTree


-- | Exercise5 

data Weekday = 
  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show,Enum)

instance Eq Weekday where
  Monday    == Monday    = True
  Tuesday   == Tuesday   = True
  Wednesday == Wednesday = True
  Thursday  == Thursday  = True
  Saturday  == Saturday  = True
  Sunday    == Sunday    = True
  _         == _         = False

instance Show Person where
   show p = forename p ++ " " ++ surname p


-- | Exercise6

instance (Eq a) => Eq (MyList a) where
   Empty == Empty = True
   Cons x _ == Cons y _ = x == y


instance (Eq a) => Eq (Tree a) where
   Null == Null = True
   Node x left right == Node y left1 right1 = x == y && left1 == left && right1 == right  
   _ == _ = False