module InsertionSort where

insertion_sort :: (Ord a) => [a] -> [a]
insertion_sort [] = []
insertion_sort (x:xs) = insert x (insertion_sort xs)

insert :: (Ord a) => a -> [a] -> [a]
insert element [] = [element]
insert element (x:xs)
    | x >= element = element:x:xs
    | otherwise = x:insert element xs