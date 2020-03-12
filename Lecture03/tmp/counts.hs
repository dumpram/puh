import StringOps
import Counts
import System.Environment
import Data.List

main :: IO ()
main = do
	xs <- getArgs
	s <- readFile (head xs)
	--putStr (unlines $ sort $ nub $ tokenize s)
	print $ count $ tokenize s
	
