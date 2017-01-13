import Data.List

count :: (Eq a) => a -> ([a] -> Int)
count x =  length . filter (==x)

histogram :: (Eq a) => [a] -> [(a, Int)]
histogram l = [(el, count el l) | el <- (nub l)]

cartesian :: [t1] -> [t] -> [(t1, t)]
cartesian xs ys = [(x, y) | x <- xs, y <- ys]

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

divides :: Integer -> Integer -> Bool
divides d x = mod x d == 0

divisors :: Integer -> [Integer]
divisors x = [d | d <- [1..x], d `divides` x]

prime :: Integer -> Bool
prime x = divisors x == [1, x]

primes :: [Integer]
primes = [x | x <- [2..], prime x]

sieve :: [Integer]
sieve = helper [2..] where
    helper (prime:xs) = prime : helper [x | x <- xs, x `mod` prime /= 0]
