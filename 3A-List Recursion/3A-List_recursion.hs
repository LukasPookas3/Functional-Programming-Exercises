{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}


ladies    = ["Mary","Kitty","Lydia","Elizabeth","Jane"]
gentlemen = ["Charles","Fitzwilliam","George","William"]

------------------------- Exercise 1

member :: [String] -> String -> Bool
member    []  _ = False
member (x:xs) y
    | x == y    = True
    | otherwise = member xs y

member' :: [String] -> String -> Bool
member'    []  _ = False
member' (x:xs) y = (x == y) || (member' xs y)

remove :: [String] -> String -> [String]
remove [] _ = []
remove (x:xs) y
    | x == y    = xs
    | otherwise = x : remove xs y

------------------------- Exercise 2

members :: [String] -> [String] -> Bool
members xs    []  = True
members xs (y:ys)
    | member xs y = members xs ys
    | otherwise   = False

members' :: [String] -> [String] -> Bool
members' xs []     = True
members' xs (y:ys) = (member xs y) && (members' xs ys)

removeAll :: [String] -> [String] -> [String]
removeAll xs []     = xs
removeAll xs (y:ys) = removeAll (remove xs y) ys


------------------------- Exercise 3

before :: [Char] -> [Char] -> Bool
before _ [] = False
before [] _ = True
before (x:xs) (y:ys)
    | x == y    = before xs ys
    | x < y     = True
    | otherwise = False

before' :: [Char] -> [Char] -> Bool
before' _ [] = False
before' [] _ = True
before' (x:xs) (y:ys) = (x < y) || (before' xs ys) -- && x == y
-- ADD THE: && x == y with: before' xs ys AS need to check every case

sorted :: [String] -> Bool
sorted []       = True
sorted [x]      = True
sorted (x:y:ys) = (before x y) && (sorted (y:ys))
-- Remember, can split a list into many individual parts by using colon repeatedly
-- CAN ALSO: recursively call list in form of - y:ys