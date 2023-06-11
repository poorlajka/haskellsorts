module SelectionSort where

selection_sort :: (Ord a) => [a] -> [a]
selection_sort [] = []
selection_sort list = [min] ++ selection_sort (remove min list)
    where min = minimum list

remove :: (Eq a) => a -> [a] -> [a]
remove target [] = error "Target not in list!"
remove target (x:xs) 
    | target == x = xs
    | otherwise = x:remove target xs