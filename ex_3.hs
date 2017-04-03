halve1 xs = (take n xs, drop n xs)
  where n = length xs `div` 2

halve2 xs = splitAt (length xs `div` 2) xs

main = print(halve2 [1, 2, 3, 4, 5, 6])
