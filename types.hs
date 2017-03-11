second xs = head (tail xs)
swap (x, y) = (y, x)
pair x y = (x, y)

double x = x * 2

add2 :: Int -> (Int -> Int)
add2 x y = x + y

numIdent :: (Num a) => a -> a
numIdent s = s



abs :: Int -> Int
abs n = if n >= 0 then n else -n

signum :: Int -> Int
signum n = if n == 0 then 0 else
              if n > 0 then 1 else -1

abs2 n | n >= 0     =  n
       | otherwise  = -n
