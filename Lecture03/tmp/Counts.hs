module Counts
	( count ) where

import Data.List (sort, group)

count :: Ord a => [a] -> [(a, Int)]
count xs = [(head zs, length zs) | zs <- group $ sort xs]
