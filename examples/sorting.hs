module Sorting where

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = (quicksort lesser) ++ [x] ++ (quicksort greater)
    where
        lesser = filter (<x) xs
        greater = filter (>=x) xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | (x<=y) = x:(merge xs (y:ys))
    | otherwise = y:(merge (x:xs) ys)

fsthalf :: [a] -> [a]
fsthalf xs = take (length xs `div` 2) xs

sndhalf :: [a] -> [a]
sndhalf xs = drop (length xs `div` 2) xs

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort (fsthalf xs)) (mergesort (sndhalf xs))

deleteFirst :: (Eq a) => a -> [a] -> [a]
deleteFirst _ [] = []
deleteFirst x (y:ys)
    | x == y = ys
    | otherwise = y : (deleteFirst x ys)

selectionsort :: (Ord a) => [a] -> [a]
selectionsort [] = []
selectionsort [x] = [x]
selectionsort xs = [min] ++ selectionsort  (deleteFirst min xs)
    where
        min = minimum xs

insertelem :: (Ord a) => a -> [a] -> [a]
insertelem x [] = [x]
insertelem x (y:ys)
    | x <= y = x:y:ys
    | otherwise = y:(insertelem x ys)

insertionsort :: (Ord a) => [a] -> [a]
insertionsort [] = []
insertionsort [x] = [x]
insertionsort (x:xs) = insertelem x (insertionsort xs)
