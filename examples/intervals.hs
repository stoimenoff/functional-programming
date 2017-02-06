before :: (Int, Int) -> (Int, Int) -> Bool
before (a, b) (c, d) = b < c

after :: (Int, Int) -> (Int, Int) -> Bool
after (a, b) (c, d) = a > d

mergeCrossing  :: (Int, Int) -> (Int, Int) -> (Int, Int)
mergeCrossing (a, b) (c, d) = ((min a c), (max b d))

insertInterval :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
insertInterval y [] = [y]
insertInterval y (x:xs)
    | before y x = y:x:xs
    | after y x = x:(insertInterval y xs)
    | otherwise = insertInterval (mergeCrossing x y) xs

unionIntervals :: [(Int, Int)] -> [(Int, Int)]
unionIntervals = foldr insertInterval []
