module Tests where

import Test.HUnit
import Homework


expr :: Expr
expr = And (Or (Val True) (Not (Val True))) (Not (And (Val True) (Val False)))



testExpr :: [Test]
testExpr = [
		TestCase (assertEqual "Neispravan 1.1!" True (eval(Val True))),
		TestCase (assertEqual "Neispravan 1.2!" False (eval(Val False))),
		TestCase (assertEqual "Neispravan 1.3!" True (eval(Or (Val True) (Val False)))),
		TestCase (assertEqual "Neispravan 1.4!" True (eval expr)),
		TestCase (assertEqual "Neispravan 1.5!" False (eval (Not expr)))
	]

testTree :: Tree Int
testTree = Node 1 (Node 2 Leaf Leaf) (Node 3 (Node 4 Leaf Leaf) Leaf)

testTreeFilter :: [Test]
testTreeFilter = [
		TestCase (assertEqual "Neispravan 2.1!" (Node 1 Leaf (Node 3 Leaf Leaf)) (treeFilter odd testTree)),
		TestCase (assertEqual "Neispravan 2.2!" (Node 1 (Node 2 Leaf Leaf) Leaf) (treeFilter (<3) testTree))
	]

testLevelMap :: [Test] 
testLevelMap = [
		TestCase (assertEqual "Neispravan 2.3!" (Node 2 (Node 2 Leaf Leaf) (Node 3 (Node 5 Leaf Leaf) Leaf)) 
			(levelMap (\l x -> if odd l then x else x + 1) testTree)),
		TestCase (assertEqual "Neispravan 2.4!" (Node 0 (Node 1 Leaf Leaf) (Node 1 (Node 2 Leaf Leaf) Leaf)) 
			(levelMap const testTree))
	]

testIsSubtree :: [Test]
testIsSubtree = [
		TestCase (assertEqual "Neispravan 2.5" True (isSubtree (Node 4 Leaf Leaf) testTree)),
		TestCase (assertEqual "Neispravan 2.6" False (isSubtree (Node 3 Leaf Leaf) testTree)),
		TestCase (assertEqual "Neispravan 2.7" True (isSubtree testTree testTree)),
		TestCase (assertEqual "Neispravan 2.8" True (isSubtree Leaf testTree))
	]

tests :: Test
tests = TestList $ testExpr ++ testTreeFilter ++ testLevelMap ++ testIsSubtree


