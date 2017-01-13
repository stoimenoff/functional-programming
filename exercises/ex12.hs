import Data.List

contains :: (Eq a) => a -> [a] -> Bool
contains el [] = False
contains el (x:xs) = el == x || contains el xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + (sum' xs)

countOccurences :: (Eq a) => a -> [a] -> Int
countOccurences el xs = length (filter (== el) xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse xs) ++ [x]

union' :: (Eq a) => [a] -> [a] -> [a]
union' xs ys = nub (xs ++ ys)

intersection' :: (Eq a) => [a] -> [a] -> [a]
intersection' xs ys = [x | x <- xs, elem x ys]

difference' :: (Eq a) => [a] -> [a] -> [a]
difference' xs ys = [x | x <- xs, not (elem x ys)]

getRow :: Int -> [[a]] -> [a]
getRow c xs = xs !! c

getColumn :: Int -> [[a]] -> [a]
getColumn c xs = map (\ ys -> ys !! c) xs

diagonal :: [[a]] -> [a]
diagonal [] = []
diagonal (xs:xss) = (head xs) : diagonal (map (drop 1) xss)

removeFirstRowAndColumn :: [[a]] -> [[a]]
removeFirstRowAndColumn [] = []
removeFirstRowAndColumn (xs:xss) = map (drop 1) xss

appendRow :: [a] -> [[a]] -> [[a]]
appendRow xs xss = xs:xss

appendColumn :: [a] -> [[a]] -> [[a]]
appendColumn xs xss = map (\(y, ys) -> y:ys) (zip xs xss)

firstColumn :: [[a]] -> [a]
firstColumn xss = map head xss

transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' xss = foldr (zipWith (:)) (repeat []) xss
