module BubbleSort where

bubble_sort :: (Ord a) => [a] -> [a]
bubble_sort [] = []
bubble_sort xs = bubble_sort (init bubbled_xs) ++ [last bubbled_xs]
    where bubbled_xs = bubble xs

bubble :: (Ord a) => [a] -> [a]
bubble [x] = [x]
bubble (x:y:xs) = [min x y] ++ bubble ([max x y] ++ xs)