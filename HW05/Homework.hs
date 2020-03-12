module Homework where


import Data.Monoid

-- | TASK01 

data Expr = And Expr Expr | Or Expr Expr | Not Expr | Val Bool

eval :: Expr -> Bool
eval (And a b) = eval a && eval b
eval (Or a b) = eval a || eval b
eval (Not a) = not (eval a)
eval (Val a) = a

-- | TASK02

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Eq, Show)

treeFilter :: (a -> Bool) -> Tree a -> Tree a
treeFilter _ Leaf = Leaf
treeFilter p (Node x left right)
	| p x = Node x (treeFilter p left) (treeFilter p right)
	| otherwise = Leaf


levelMap :: (Int -> a -> b) -> Tree a -> Tree b
levelMap = levelMap' 0 

levelMap' :: Int -> (Int -> a -> b) -> Tree a -> Tree b
levelMap' _ _ Leaf = Leaf
levelMap' l p (Node x left right) = Node (p l x) (levelMap' (l + 1) p left) (levelMap' (l + 1) p right)


isSubtree :: Eq a => Tree a -> Tree a -> Bool
isSubtree _ Leaf = False
isSubtree Leaf _ = True
isSubtree test@(Node y l2 r2) (Node x l1 r1) = y == x && l1 == l2 && r1 == r2 || isSubtree test l1 || isSubtree test r1 

-- | TASK04

data DiffList a = DiffList {
	undiff :: [a] -> [a]
} 

testList :: DiffList Int
testList = DiffList {
	undiff = \xs -> [1,2,3] ++ xs
}

empty :: DiffList a
empty = DiffList {
	undiff = \xs -> [] ++ xs 
}

fromList :: [a] -> DiffList a
fromList ys = DiffList {
	undiff = \xs -> ys ++ xs
}

toList :: DiffList a -> [a]
toList dl = undiff dl []

append :: DiffList a -> DiffList a -> DiffList a
append dl1 dl2 = DiffList {
	undiff = undiff dl1 . undiff dl2
}

instance (Show a) => Show (DiffList a) where
	show dl = show (toList dl)

instance Monoid (DiffList a) where
	mempty = empty
	mappend = append


