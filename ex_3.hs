halve1 xs = (take n xs, drop n xs)
  where n = length xs `div` 2

main = print(halve1 [1, 2, 3, 4, 5, 6])
