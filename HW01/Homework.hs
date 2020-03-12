import  Data.List

-- TASK1

-- Returns 2D vector norm.
norm (x, y) = sqrt (x * x + y * y)
-- Returns normalized 2D vector. 
normalize (x, y) = if isNull(x,y) then error "Cannot normalize null vector!" else (x / norm(x,y), y / norm(x,y))
-- Returns vector multiplied by scalar.
scalarMult a (x, y) = (a * x, a * y)
-- Returns scalar which represents result of scalar multiplication.
dot (x, y) (u, v) = x * u + y * v
-- Returns cosine function of angle between vectors (x, y) and (u, v) respectively.
cos' (x, y) (u, v)	
	| isNull(x,y) || isNull(u,v) = error "Null vector given!"
	| otherwise = dot (x, y) (u, v) / (norm (x, y) * norm (u, v))
-- Returns True if vectors are parallel, False otherwise.
areParallel (x, y) (u, v) = cos' (x, y) (u, v) >= 0.9999
-- Returns True if vector given is null.
isNull (x, y) = x == 0 && y == 0


-- TASK2

-- Calculates ranking of 3 players.
finalRanking (n1, s1) (n2, s2) (n3, s3) 
	| s1 >= s2 && s2 >= s3 = sortAlpha (n1, s1) (n2, s2) (n3, s3)
	| s1 >= s3 && s3 >= s2 = sortAlpha (n1, s1) (n3, s3) (n2, s2)
	| s2 >= s1 && s1 >= s3 = sortAlpha (n2, s2) (n1, s1) (n3, s3)
	| s2 >= s3 && s3 >= s1 = sortAlpha (n2, s2) (n3, s3) (n1, s1)
	| s3 >= s1 && s1 >= s2 = sortAlpha (n3, s3) (n1, s1) (n2, s2)
	| s3 >= s2 && s2 >= s1 = sortAlpha (n3, s3) (n2, s2) (n1, s1)

-- Sorts players by name in case of equal score.					
sortAlpha (n1, s1) (n2, s2) (n3, s3) 	
	| s1 == s2 && s2 /= s3 = if n1 < n2 then [n1, n2, n3] else [n2, n1, n3]
	| s1 /= s2 && s2 == s3 = if n2 < n3 then [n1, n2, n3] else [n1, n3, n2]
	| s1 /= s2 && s2 /= s3 = [n1, n2, n3]
	| n1 < n2 && n2 < n3 = [n1, n2, n3]
	| n1 < n3 && n3 < n2 = [n1, n3, n2]
	| n2 < n1 && n1 < n3 = [n2, n1, n3]
	| n2 < n3 && n3 < n1 = [n2, n3, n1]
	| n3 < n1 && n1 < n2 = [n3, n1, n2]
	| n3 < n2 && n2 < n1 = [n3, n2, n1]


-- TASK3

-- Function calculates number of moves that are necessary to move
-- all files in respective directories. 
engine a b c x y z = 
	check b x + check c x +
	check a y + check c y +
	check a z + check b z

-- Checks how many files with given extension are in list.
check l s = length [x | x <- words l, isSuffixOf s x]

-- Finds minimum element in list.
findMin list = head $ sort $ zip list [1..]

-- Returns string depending on code of permutation.
decode (value, code) 
	| code == 1 = show (value) ++ "\nimage\naudio\nvideo"
	| code == 2 = show (value) ++ "\nimage\nvideo\naudio"
	| code == 3 = show (value) ++ "\naudio\nimage\nvideo"
	| code == 4 = show (value) ++ "\naudio\nvideo\nimage"
	| code == 5 = show (value) ++ "\nvideo\nimage\naudio"
	| code == 6 = show (value) ++ "\nvideo\naudio\nimage"

-- Main function handles IO operations.
main = do
	a <- getLine
	b <- getLine
	c <- getLine
	putStrLn(decode $ findMin 
		[engine a b c "png" "mp3" "mp4",
		engine a b c "png" "mp4" "mp3",
		engine a b c "mp3" "png" "mp4",
		engine a b c "mp3" "mp4" "png",
		engine a b c "mp4" "png" "mp3", 
		engine a b c "mp4" "mp3" "png"]);
