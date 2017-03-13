-- this is syntactic sugar for
-- add x y = x + y
-- add = \x y -> x + y
add = \x -> (\y -> x + y)
