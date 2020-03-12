-- returns 2D vector norm
norm (x, y) = sqrt (x * x + y * y)
-- returns normalized 2D vector 
normalize (x, y) = if isNull(x,y) then error "Cannot normalize null vector!" else (x / norm(x,y), y / norm(x,y))
-- returns vector multiplied by scalar
scalarMult a (x, y) = (a * x, a * y)
-- returns scalar which represents result of scalar multiplication
dot (x, y) (u, v) = x * u + y * v
-- returns cosine function of angle between vectors (x, y) and (u, v) respectfully
cos' (x, y) (u, v)	| isNull(x,y) || isNull(u,v) = error "Null vector given!"
			| otherwise = dot (x, y) (u, v) / (norm (x, y) * norm (u, v))
-- returns True if vectors are parallel, False otherwise
areParallel (x, y) (u, v) = cos' (x, y) (u, v) >= 0.9999
-- returns True if vector given is null
isNull (x, y) = x == 0 && y == 0

