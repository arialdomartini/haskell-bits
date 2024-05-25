cproduct [] = 1
cproduct (x : xs) = x * cproduct xs

main = print(cproduct [2, 3, 4]) -- should be 24
