

------------------------- Exercise 1

square :: Int -> Int
square x = x * x

pythagoras :: Int -> Int -> Int -> Bool
pythagoras a b c = square a + square b == square c


------------------------- Exercise 2

factorial :: Int -> Int
factorial x
    | x <= 1    = 1
    | otherwise = x * factorial (x-1)

-- ERROR MESSAGE - consider the case of if there isn't a GCD
-- That's why we do <= 0

euclid :: Int -> Int -> Int
euclid x y
    | x <= 0 || y <= 0 = error "Invalid input"
    | x == y = x
    | x <  y = euclid x (x - y)
    | x >  y = euclid (x - y) y

power :: Int -> Int -> Int
power x y
--  | y < 0 = error "Negative exponent" - think about errors too
    | y == 0 = 1
    | otherwise = x * power x (y - 1)
--note: you will need to create your own cases,
--      replacing the equals (=) sign with guards


------------------------- Exercise 3

range :: Int -> Int -> [Int]
range n m
    | n > m = []
    | otherwise = n : range (n + 1) m
--note: you will need to create your own guards
--      and add your own parameters

times :: [Int] -> Int
times [] = 1
times (x:xs) = x * times xs
-- times x:xs
--     | otherwise = x * times x:(xs - 1)
-- ABOVE IS ORIGINAL ANSWER - don't always need to use guards AND don't think can do function x:xs again?

--note: you will need to create your own pattern-matching

fact :: Int -> Int
fact x = times (range 1 x)
-- ABOVE DOESN'T WORK WITHOUT BRACKETS AROUND SECOND THINGY