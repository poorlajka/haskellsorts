module MergeSort where

merge_sort :: (Ord a) => [a] -> [a]
merge_sort [x] = [x]
merge_sort xs = merge (merge_sort left_xs) (merge_sort right_xs)
    where (left_xs, right_xs) = splitAt ((length xs + 1) `div` 2) xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge [x] [] = [x]
merge [] [y] = [y]
merge (x:xs) (y:ys)
    | x < y = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys
