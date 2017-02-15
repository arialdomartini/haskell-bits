rqsort [] = []
rqsort (x : xs) = rqsort larger ++ [x] ++ rqsort smaller
                    where
                      smaller = [e | e <- xs, e < x]
                      larger = [e | e <- xs, e > x]
main = print(rqsort [1, 3, 2, 5, 0, 10]) -- should be [10, 5, 4, 2, 1 0]
