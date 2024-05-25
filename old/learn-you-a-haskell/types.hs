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

signum2 :: Int -> Int
signum2 n |  n == 0  = 0
          |  n > 0   = 1
          |  n < 0   = -1


-- Pattern matching
not :: Bool -> Bool
not False = True
not True  = False

(&&)          :: Bool -> Bool -> Bool
True  && True  = True
True  && False = False
False && True  = False
False && False = False

(&&&)        :: Bool -> Bool -> Bool
True &&& True = True
_    &&& _    = False

(&&&&)      :: Bool -> Bool -> Bool
True  &&&& b = b
False &&&& b = False


mhead      :: [a] -> a
mhead (x:_) = x

mtail       :: [a] -> [a]
mtail (_:xs) = xs

section_halv = (/2)
section_double = (*2)
section_successor = (+1)
section_reciprocate = (1/)
