calculate_sum [] = 0
calculate_sum (x : xs) = x + calculate_sum xs

main = print(calculate_sum [1, 10, 20, 4])  -- should e 35
