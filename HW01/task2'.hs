finalRanking (n1, s1) (n2, s2) (n3, s3) | s1 >= s2 && s2 >= s3 = sortAlpha (n1, s1) (n2, s2) (n3, s3)
					| s1 >= s3 && s3 >= s2 = sortAlpha (n1, s1) (n3, s3) (n2, s2)
					| s2 >= s1 && s1 >= s3 = sortAlpha (n2, s2) (n1, s1) (n3, s3)
					| s2 >= s3 && s3 >= s1 = sortAlpha (n2, s2) (n3, s3) (n1, s1)
					| s3 >= s1 && s1 >= s2 = sortAlpha (n3, s3) (n1, s1) (n2, s2)
					| s3 >= s2 && s2 >= s1 = sortAlpha (n3, s3) (n2, s2) (n1, s1)
					
sortAlpha (n1, s1) (n2, s2) (n3, s3) 	| s1 == s2 && s2 /= s3 = if n1 < n2 then [n1, n2, n3] else [n2, n1, n3]
					| s1 /= s2 && s2 == s3 = if n2 < n3 then [n1, n2, n3] else [n1, n3, n2]
					| s1 /= s2 && s2 /= s3 = [n1, n2, n3]
					| n1 < n2 && n2 < n3 = [n1, n2, n3]
					| n1 < n3 && n3 < n2 = [n1, n3, n2]
					| n2 < n1 && n1 < n3 = [n2, n1, n3]
					| n2 < n3 && n3 < n1 = [n2, n3, n1]
					| n3 < n1 && n1 < n2 = [n3, n1, n2]
					| n3 < n2 && n2 < n1 = [n3, n2, n1]


