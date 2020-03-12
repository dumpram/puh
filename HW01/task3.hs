import  Data.List

engine a b c x y z = 
	check b x + check c x +
	check a y + check c y +
	check a z + check b z

check l s = length [x | x <- words l, isSuffixOf s x]

decode code | snd code == 1 = show (fst code) ++ "\nimage\naudio\nvideo"
			| snd code == 2 = show (fst code) ++ "\nimage\nvideo\naudio"
			| snd code == 3 = show (fst code) ++ "\naudio\nimage\nvideo"
			| snd code == 4 = show (fst code) ++ "\naudio\nvideo\nimage"
			| snd code == 5 = show (fst code) ++ "\nvideo\nimage\naudio"
			| snd code == 6 = show (fst code) ++ "\nvideo\naudio\nimage"
findMin list = head $ sort $ zip list [1..]

main = do
	a <- getLine
	b <- getLine
	c <- getLine
	putStrLn(decode $ findMin [engine a b c "png" "mp3" "mp4",
		engine a b c "png" "mp4" "mp3",
		engine a b c "mp3" "png" "mp4",
		engine a b c "mp3" "mp4" "png",
		engine a b c "mp4" "png" "mp3", 
		engine a b c "mp4" "mp3" "png"]);
