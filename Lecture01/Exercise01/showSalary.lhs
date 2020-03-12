> showSalary amount bonus = if (amount + bonus) < 0 then "Salary is negative!!" else "Salary is " ++ show amount ++ (if bonus /= 0 then ", and a bonus is " ++ show bonus else "") ++ "."   
