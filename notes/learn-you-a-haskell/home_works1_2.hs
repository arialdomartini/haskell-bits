eval_last_1 xs = drop (length xs - 1) xs                -- ok, O(n)
eval_last_2 xs = head (drop (length xs -1 ) xs )        -- not ok, since it returns a char rather than a string
eval_last_3 xs = tail (reverse xs)                      -- not ok, tail returns 2 elements
eval_last_4 xs = reverse (head xs)                      -- not ok, reversing a the "a", which is "a"
eval_last_5 xs = xs !! (length xs -1)                   -- ok, O(n)
eval_last_6 xs = head (drop (length xs) xs )            -- not ok, drop drops too many items so head is applied to an empty list
eval_last_7 xs = head (reverse xs)                      -- Not ok, head returns a char, not a string
eval_last_8 xs = reverse xs !! (length xs - 1)          -- Not ok, should be !! 0, otherwise it selects the first element

main = print(eval_last_8 ["a", "b", "c"])
