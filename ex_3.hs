halve1 xs = (take n xs, drop n xs)
  where n = length xs `div` 2

halve2 xs = splitAt (length xs `div` 2) xs

halve3 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs

main = print(halve3 [1, 2, 3, 4, 5, 6])
