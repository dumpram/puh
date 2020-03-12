module Ratio1 where

data Ratio = Ratio Int Int  

(%) :: (Integral a, Integral b) => a -> b -> Ratio
(%) x y = Ratio (fromIntegral x) (fromIntegral y)
(%) _ 0 = error "Division by zero"

instance Eq Ratio where
	Ratio a b == Ratio c d = a == c && b == d

instance Ord Ratio where
	Ratio a b <= Ratio c d = a * d <= b * c

instance Num Ratio where
	Ratio a b + Ratio c d = Ratio (a * d + b * c) (b * d)
	Ratio a b - Ratio c d = Ratio (a * d - b * c) (b * d)
	Ratio a b * Ratio c d = Ratio (a * c) (b * d)
	abs (Ratio a b) = Ratio (abs a) (abs b)
	signum (Ratio a b) = Ratio (signum a) (signum b)
	negate (Ratio a b) = Ratio (a * (-1)) b
	fromInteger a = Ratio (fromIntegral a) 1

instance Show Ratio where
	show (Ratio a b) = show (quot a gd) ++ " % " ++ show (quot b gd)
		where gd = gcd a b

specialFunction :: a -> a
specialFunction = undefined