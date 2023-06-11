module QuickSort where

quick_sort :: (Ord a) => [a] -> [a]
quick_sort [] = []
quick_sort (x:xs) = 
    quick_sort (filter (<=x) xs) ++ [x] ++ quick_sort (filter (>x) xs)