module FFT where

type Complex = (Double, Double)

cadd :: Complex -> Complex -> Complex
cadd (x,y) (u,v) = (u + x, v + y)

csub :: Complex -> Complex -> Complex
csub (x,y) (u,v) = (x - u, y - v)

cmul :: Complex -> Complex -> Complex
cmul (x,y) (u,v) = (u * x - y * v, y * u + x * v)

cdiv :: Complex -> Complex -> Complex
cdiv (x,y) (u,v) = ((x*u + y*v)/k, (-x*v + y*u)/k)
	where 
		k = u**2 + y**2


fft :: [Complex] -> [Complex]
fft xs = fft' (length xs) (takeEven xs) (takeOdd xs) 
	
fft' :: Int -> [Complex] -> [Complex] -> [Complex]
fft' 2 evens odds = [cadd v1 v2, csub v1 v2]
	where
		v1 = head evens
		v2 = head odds
fft' n evens odds = map (\k -> cadd (fk1 !! (k `rem` n1)) (cmul (fk2 !! (k `rem` n1)) (wnk k))) [0..n - 1]
	where 
		fk1 = fft' (quot n 2) (takeEven evens) (takeOdd evens) 
		fk2 = fft' (quot n 2) (takeEven odds) (takeOdd odds)
		wnk k = (cos(2 * pi * rac k n), -sin(2 * pi * rac k n))
		rac a b = fromIntegral a / fromIntegral b
		n1 = quot n 2

takeEven :: [Complex] -> [Complex]
takeEven [] = []
takeEven [x] = [x]
takeEven (x:_:xs) = x:takeEven xs

takeOdd :: [Complex] -> [Complex]
takeOdd [] = [] 
takeOdd [_] = []
takeOdd (_:x:xs) = x:takeOdd xs