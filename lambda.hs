-- this is syntactic sugar for
-- add x y = x + y
-- add = \x y -> x + y
add = \x -> (\y -> x + y)


const :: a -> b -> a
const a b = a

const2 :: a -> (b -> a)
const2 = \a -> (\_ -> a)
