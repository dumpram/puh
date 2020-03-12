module StringOps 
	(tokenize 
	, removePunctuation 
	, cleanWord)  where 

import Data.Char

--Tokenizes string.
tokenize :: String -> [String]
tokenize s = 
	[ w' | w <- words s, let w' = cleanWord w, w' /= ""];

-- Lowerizes it.
lowercase :: String -> String
lowercase s = [toLower c | c <- s]

-- Cleans word.
cleanWord :: String -> String
cleanWord w = removePunctuation $ lowercase w

-- Removes punctuation.
removePunctuation :: String -> String
removePunctuation w =
	 [ c | c <- w, not $ isPunctuation c]
