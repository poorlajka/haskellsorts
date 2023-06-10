module Main where

main :: IO ()
main = print (selection_sort [5,1, 3,2])

selection_sort :: (Ord a) => [a] -> [a]
selection_sort [] = []
selection_sort list = [min] ++ selection_sort (remove min list)
    where min = minimum list

remove :: (Eq a) => a -> [a] -> [a]
remove target [] = error "Element not found!"
remove target (x:xs) 
    | target == x = xs
    | otherwise = x:remove target xs

insertion_sort :: (Ord a) => [a] -> [a]
insertion_sort [] = []
insertion_sort [x] = [x]
insertion_sort (x:xs) = insert x (insertion_sort xs)

insert :: (Ord a) => a -> [a] -> [a]
insert element [] = [element]
insert element (x:xs)
    | x >= element = element:x:xs
    | otherwise = x:insert element xs

bubble_sort :: (Ord a) => [a] -> [a]
bubble_sort [] = []
bubble_sort list = bubble_sort (init bubbled_list) ++ [last bubbled_list]
    where bubbled_list = bubble list

bubble :: (Ord a) => [a] -> [a]
bubble [x] = [x]
bubble (x:y:xs) = [min x y] ++ bubble ([max x y] ++ xs)

    
    