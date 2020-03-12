-- Exercise 1

-- Concatenates lists given, but leaves out second list if it has
-- less than 2 elements.
concat3 first second third = 
	first ++ (if length second < 2 then "" else second) ++ third

-- Shows salary. 
showSalary amount bonus = 
	if (amount + bonus) < 0 
		then "Salary is negative!!" 
		else "Salary is " ++ show amount ++ 
			(if bonus /= 0 
				then ", and a bonus is " ++ show bonus 
				else "") ++ "."   
