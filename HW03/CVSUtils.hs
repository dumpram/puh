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

parseCSV :: Separator -> Document -> CSV
parseCSV sep d
	| not (isWellFormed csv) = error "The CSV file is not well-formed"
	| otherwise = csv
	where csv = map (splitOn sep) (lines d)

isWellFormed :: CSV -> Bool
isWellFormed csv
	| length csv == 1 && null(head csv) = False
	| otherwise = length(nub (map length csv)) == 1 

showCSV :: Separator -> CSV -> Document
showCSV sep csv 
	| not (isWellFormed csv) = error "The CSV file is not well-formed"
	| otherwise = intercalate "\n" (map(intercalate sep) csv)

sizeCSV :: CSV -> Int
sizeCSV csv 
	| not (isWellFormed csv) = error "The CSV file is not well formed"
	| otherwise = length (head csv)

colFields :: Int -> CSV -> [Field]
colFields i csv 
	| not (isWellFormed csv) = error "The CSV file is not well-formed"
	| i < 0 = error "Negative index provided. Positive indices allowed"
	| i >= sizeCSV csv = error ("There is no column " ++ show i ++ " in the CSV document")
	| otherwise = [entry !! i | entry <- csv]

readCSV :: Separator -> FilePath -> IO CSV
readCSV sep path = do
	d <- readFile path
	return (parseCSV sep d)

writeCSV :: Separator -> FilePath -> CSV -> IO ()
writeCSV sep path csv = writeFile path (showCSV sep csv)