module Numbers where

digit :: Integer -> Bool
digit x = (abs x) < 10

cutLastDigit :: Integer -> Integer
cutLastDigit x = div x 10

lastDigit :: Integer -> Integer
lastDigit x = mod x 10

digits :: Integer -> [Integer]
digits x
    | digit x   = [x]
    | otherwise = (digits (cutLastDigit x)) ++ [lastDigit x]

countDigits :: Integer -> Int
countDigits x = length (digits x)

sumDigits :: Integer -> Integer
sumDigits x = sum (digits x)

divides :: Integer -> Integer -> Bool
divides d x = mod x d == 0

divisors :: Integer -> [Integer]
divisors x = [d | d <- [1..x], d `divides` x]

prime :: Integer -> Bool
prime x = divisors x == [1, x]

isDivisorOf :: Integer -> Integer -> Bool
isDivisorOf x d = mod x d == 0

divisors' :: Integer -> [Integer]
divisors' x = filter (isDivisorOf x) [1..x]

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (fibonacci (n - 1)) + (fibonacci (n - 2))

iterFibonacci :: Integer -> Integer
iterFibonacci n = go n 0 1
    where
        go 0 previous current = previous
        go n previous current = go (n - 1) current (current + previous)

square :: Double -> Double
square x = x * x

average :: Double -> Double -> Double
average x y = (x + y) / 2

preciseSquareRoot :: Double -> Double -> Double
preciseSquareRoot precision x = guessRoot 1
    where
        guessRoot guess
            | closeEnough = guess
            | otherwise   = guessRoot improvedGuess
            where
                closeEnough = abs ((square guess) - x) < precision
                improvedGuess = average guess  (x / guess)

squareRoot :: Double -> Double
squareRoot x = preciseSquareRoot 0.000001 x
