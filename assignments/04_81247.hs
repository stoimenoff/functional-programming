dropFirstLast :: [a] -> [a]
dropFirstLast [] = []
dropFirstLast [x] = []
dropFirstLast xs = tail $ init $ xs

medianOfSorted :: (Ord a) => [a] -> a
medianOfSorted [] = error "Empty list!"
medianOfSorted [x] = x
medianOfSorted (x:y:[]) = y
medianOfSorted xs = medianOfSorted $ dropFirstLast $ xs

ascendingPrefix :: (Ord a) => [a] -> [a]
ascendingPrefix [] = []
ascendingPrefix [x] = [x]
ascendingPrefix (x:y:ys)
    | x < y = x : (ascendingPrefix (y:ys))
    | otherwise = [x]

stripPrefix :: (Eq a) => [a] -> [a] -> [a]
stripPrefix [] xs = xs
stripPrefix _ [] = []
stripPrefix (p:ps) (x:xs)
    | x == p = stripPrefix ps xs
    | otherwise = error "Prefix doesn't match!"

splitAscending :: (Ord a) => [a] -> [[a]]
splitAscending [] = []
splitAscending xs = prefix : (splitAscending sufix) where
    prefix = ascendingPrefix xs
    sufix = stripPrefix prefix xs

solution :: (Ord a) => [a] -> [a]
solution = (map medianOfSorted) . splitAscending
