
import Data.Char

-- LIBRARY FUNCTIONS:
-- and: returns whether all values in boolean list is True
-- or: like and but OR operation instead
-- any: returns True if at least one item in list fulfills condition. Example: any even [1,3,5,7,9] - False, any (1==) [0,1,2,3,4,5] - True
-- all: returns True is ALL items in list fulfills condition
-- maximum: returns max value in list
-- minimum: returns min value in list
-- product: returns product of list
-- sum: returns sum of list
-- concat: accepts a LIST OF LISTS and concats them

-- max: returns larger of its two arguments
-- min: same as max but for minimum

------------------------- a

allTrue1 :: [Bool] -> Bool
allTrue1 [] = True
allTrue1 (x:xs) = x && (allTrue1 xs)

allTrue2 :: [Bool] -> Bool
allTrue2 xs = and xs

allTrue3 :: [Bool] -> Bool
allTrue3 xs = foldr (&&) True xs

allTrue4 :: [Bool] -> Bool
allTrue4 xs = foldl (&&) True xs

------------------------- b

longestLength1 :: [[a]] -> Int
longestLength1 [x]    = length x
longestLength1 (x:xs) = max (length x) (longestLength1 xs)
-- recursively calls, ends on length x to give length for max function

longestLength2 :: [[a]] -> Int
longestLength2 xs = maximum [length x | x <- xs]

longestLength3 :: [[a]] -> Int
longestLength3 xs = foldl1 max (map length xs)
-- MAYBE?

longestLength4 :: [[a]] -> Int
longestLength4 = foldr1 max . map length


------------------------- c

sumOddSquares1 :: [Int] -> Int
sumOddSquares1 [] = 0
sumOddSquares1 (x:xs)
    | odd x     = x*x + sumOddSquares1 xs
    | otherwise = sumOddSquares1 xs

sumOddSquares2 :: [Int] -> Int
sumOddSquares2 xs = sum [x*x | x <- xs , odd x]

sumOddSquares3 :: [Int] -> Int
sumOddSquares3 xs = foldl (+) 0 (map (^2) (filter odd xs))

sumOddSquares4 :: [Int] -> Int
sumOddSquares4 xs = foldr (+) 0 (map (^2) (filter odd xs))

-- GET RID OF MAP:

sumOddSquares5 :: [Int] -> Int
sumOddSquares5 xs = foldr ((+) . (^2)) 0 (filter odd xs)

-- REMEMBER: if something doesn't work, try wrapping around - DON'T BE SILLY, WRAP THE FUNCTION ALWAYS YES COMPRENDO?
-- also: don't use x in map function thingy, unless defined function

-- CAN USE FOLDR, FOLDL, FOLDR1, FOLDL1 to basically CREATE LIBRARY FUNCTIONS

------------------------- d

shortFWords1 :: [String] -> Bool
shortFWords1 [] = False
shortFWords1 (x:xs)
    | ((length x) == 4) && ((head x) == 'F') = True
    | otherwise                              = shortFWords1 xs
-- if need to get head or tail in a LIST OF LISTS, use 'head' or 'tail' keywords

shortFWords2 :: [String] -> Bool
shortFWords2 xs = or [ (head x) == 'F' | x <- xs , (length x) == 4 ]
-- Can do list comprehension for something like this, create a list that FILTERS then find one you want (single out values from list as seen before / previously)
-- If do list comprehension, should return booleans

shortFWords3 :: [String] -> Bool
shortFWords3 xs = foldl (||) False (map (f) (filter ((=='F') . head) xs))
    where
        f x = (length x == 4)

shortFWords4 :: [String] -> Bool
shortFWords4 xs = foldl (||) False (map (f1) (filter (f2) xs))
    where
        f1 x = (length x == 4)
        f2 x = (head x == 'F')
-- FOR FOLDL etc, USE MAP, can't use filter alone - WRONG

shortFWords5 :: [String] -> Bool
shortFWords5 xs = foldl (||) False (map ((==4) . length) (filter ((=='F') . head) xs))
-- can't use x, so apply two functions together

------------------------- e

wordScore1 :: String -> Int
wordScore1 [] = 0
wordScore1 (x:xs)
    | 1 <= i, i <= 26 = i + wordScore1 xs
    | otherwise = wordScore1 xs
  where
    i = subtract 64 (ord (toUpper x))
-- can use 'where' clause to shorten stuff

wordScore2 :: String -> Int
wordScore2 xs = sum [ subtract 64 (ord (toUpper x)) | x <- xs , subtract 64 (ord (toUpper x)) <= 26 , subtract 64 (ord (toUpper x)) >= 1 ]
-- type of initial x should carry over

wordScore3 :: String -> Int
-- wordScore3 xs = foldl (+) 0 (map (toUpper) (filter (f) xs))
--     where
--         f x = ((subtract 64 (ord x)) <= 26) || ((subtract 64 (ord x)) >= 1)
wordScore3 = foldl (+) 0 . filter (1 <=) . filter (<= 26) .  map (subtract 64 . ord .  toUpper)
-- REMEMBER: filter is for conditions, map is for functions

------------------------- f

concatCheapWords1 :: [String] -> String
concatCheapWords1 [] = []
concatCheapWords1 (x:xs)
    | (wordScore1 x) <= 42 = ' ' : x ++ concatCheapWords1 xs
    | otherwise            = concatCheapWords1 xs
-- use ++ for lists and strings etc and use + for integers

concatCheapWords2 :: [String] -> String
concatCheapWords2 xs = concat [ ' ' : x | x <- xs , (wordScore1 x) <= 42 ]

-- Library functions in front of list comprehension

concatCheapWords3 :: [String] -> String
concatCheapWords3 xs = foldl (++) [] (map (f1) (filter (f2) xs))
    where
        f1 x = ' ' : x
        f2 x = (wordScore1 x) <= 42

concatCheapWords4 :: [String] -> String
concatCheapWords4 = foldl (++) [] . map (' ':) . filter ((<=42) . wordScore1)