module CSVUtils 
	(Separator
	, Document
	, CSV
	, Entry
	, Field
	, parseCSV
	, showCSV
	, colFields
	, readCSV
	, writeCSV) where 

import Data.List.Split
import Data.List

type Separator = String
type Document = String
type CSV = [Entry]
type Entry = [Field]
type Field = String

doc :: Document
doc = "John;Doe;15\nTom;Sawyer;12\nAnnie;Blake;20"

brokenDoc :: Document
brokenDoc = "One;Two\nThree;Four;Five"

-- Function parses String and returns CSV.
parseCSV :: Separator -> Document -> CSV
parseCSV sep d
	| not (isWellFormed csv) = error "The CSV file is not well-formed"
	| otherwise = csv
	where csv = map (splitOn sep) (lines d)

-- Checks if CSV is well formed.
isWellFormed :: CSV -> Bool
isWellFormed csv
	| length csv == 1 && null(head csv) = False
	| otherwise = length(nub (map length csv)) == 1 

-- Shows CSV with given separator.
showCSV :: Separator -> CSV -> Document
showCSV sep csv 
	| not (isWellFormed csv) = error "The CSV file is not well-formed"
	| otherwise = intercalate "\n" (map(intercalate sep) csv)

-- Returns size of CSV(number of columns).
sizeCSV :: CSV -> Int
sizeCSV csv 
	| not (isWellFormed csv) = error "The CSV file is not well formed"
	| otherwise = length (head csv)

-- Returns column with given index.
colFields :: Int -> CSV -> [Field]
colFields i csv 
	| not (isWellFormed csv) = error "The CSV file is not well-formed"
	| i < 0 = error "Negative index provided. Positive indices allowed"
	| i >= sizeCSV csv = error ("There is no column " ++ show i ++ " in the CSV document")
	| otherwise = [entry !! i | entry <- csv]

-- Reads CSV with given separator from file.
readCSV :: Separator -> FilePath -> IO CSV
readCSV sep path = do
	d <- readFile path
	if head sep `notElem` d then error ("The character '" ++ sep ++ "' doesn't occur in text.") else return (parseCSV sep d)

-- Writes given CSV with given separator to file.
writeCSV :: Separator -> FilePath -> CSV -> IO ()
writeCSV sep path csv = writeFile path (showCSV sep csv)