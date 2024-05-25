-- this is syntactic sugar for
-- add x y = x + y
-- add = \x y -> x + y
add = \x -> (\y -> x + y)


const :: a -> b -> a
const a b = a

const2 :: a -> (b -> a)
const2 = \a -> (\_ -> a)

const3 x = \_ -> x


odds n = map f [0..n-1]
         where
           f x = x*2 + 1

oddsl n = map (\x -> x*2 + 1) [0..n-1]
