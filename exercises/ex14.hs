import Data.List
import Data.Char

count :: (Eq a) => a -> [a] -> Int
count n = length . filter (== n)

multiUnion :: (Eq a) => [a] -> [a] -> [a]
multiUnion xs ys = concat [(replicate (count x elements) x) | x <- nub elements]
    where elements = xs ++ ys

multiIntersection :: (Eq a) => [a] -> [a] -> [a]
multiIntersection xs ys = concat [ (replicate (min (count x xs) (count x ys)) x) | x <- nub xs]

notNegative :: Int -> Int
notNegative x
    | x >= 0 = x
    |otherwise = 0

multiDifference :: (Eq a) => [a] -> [a] -> [a]
multiDifference xs ys = concat [(replicate (notNegative ((count x xs) - (count x ys))) x) | x <- nub xs]

divides :: Int -> Int -> Bool
divides d x = mod x d == 0

divisors :: Int -> [Int]
divisors x = [d | d <- [1..x], d `divides` x]

prime :: Int -> Bool
prime x = divisors x == [1, x]

goldbach :: Int -> [(Int, Int)]
goldbach n = [(x, n - x) | x <- [2..n-2], prime x, prime (n-x)]

encodeLetter :: Int -> Char -> Char
encodeLetter n letter
    | n > 26 || n < 0 = encodeLetter (abs (mod n 26)) letter
    | isLetter (chr code) = chr code
    | otherwise = chr (code - 26)
        where code = n + (ord letter)

encodeIfLetter :: Int -> Char -> Char
encodeIfLetter n symbol
    | isLetter symbol = encodeLetter n symbol
    | otherwise = symbol

cesarCypher :: Int -> String -> String
cesarCypher n = map (encodeIfLetter n)

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 el xs = el : xs
insertAt n el (x:xs) = x : (insertAt (n - 1) el xs)

allInsertions :: a -> [a] -> [[a]]
allInsertions el xs = zipWith3 insertAt [0..(length xs)] (repeat el) (repeat xs)

allInsertionsInLists :: a -> [[a]] -> [[a]]
allInsertionsInLists _ [] = []
allInsertionsInLists x (xs:xss) = allInsertions x xs ++ allInsertionsInLists x xss

permute :: [a] -> [[a]]
permute [] = [[]]
permute (x:xs) = allInsertionsInLists x (permute xs)

insertInAll :: a -> [[a]] -> [[a]]
insertInAll x = map ((:) x)

combinations :: Int -> [a] -> [[a]]
combinations _ [] = [[]]
combinations 0 (x:xs) = []
combinations 1 (x:xs) = map (\el -> [el]) (x:xs)
combinations n (x:xs)
    | n > length (x:xs) = []
    | otherwise = insertInAll x (combinations (n - 1) xs) ++ combinations n xs
