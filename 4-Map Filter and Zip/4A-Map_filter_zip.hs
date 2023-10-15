
zipWithCopy :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithCopy f [] _          = []
zipWithCopy f _ []          = []
zipWithCopy f (x:xs) (y:ys) = (f x y) : (zipWithCopy f xs ys)
-- function a->b->c depends on other inputs and output as well


------------------------- Exercise 1

doubles :: [Int] -> [Int]
doubles []     = []
doubles (x:xs) = (2*x) : (doubles xs)
-- doubles xs     = map (2 *) xs
-- NOTE: above, use of map -> can use map for any arithmetic, including numbers

odds :: [Int] -> [Int]
odds [] = []
odds (x:xs)
    | odd x     = x : odds xs
    | otherwise = odds xs

odds' :: [Int] -> [Int]
odds' xs = filter odd xs
-- FILTER FUNCTIONS need to return a boolean (example: odd returns a boolean)

doubleodds :: [Int] -> [Int]
doubleodds xs = doubles (odds xs)

doubleodds' :: [Int] -> [Int]
doubleodds' [] = []
doubleodds' (x:xs)
    | odd x     = (2*x) : doubleodds' xs
    | otherwise = doubleodds' xs

doubleodds'' :: [Int] -> [Int]
-- doubleodds'' xs = map doubles (odds xs)
-- doubleodds'' xs = map (2* xs) . (odds xs)
-- doubleodds'' = map (2*) . (odds)
doubleodds'' xs = map (2*) (odds xs)
-- NOTE FOR MAP:
-- - try not to use xs or something unless have to (didn't work here)
-- - use . to map 2 functions together
-- - KIND OF BASICALLY IGNORE THE ABOVE 2 POINTS


------------------------- Exercise 2

-- BUILT-IN FUNCTIONS:
-- - odd
-- - even
-- - length
-- - sum

shorts :: [String] -> [String]
shorts = filter tooLong
    where
        tooLong x = length x <= 5
-- Don't need to define functions for 'filter', think about 'filter''s code

squarePositives :: [Int] -> [Int]
-- squarePositives = map (x * x) . (filter positives)
-- x not defined so have to use 'where' clause
squarePositives xs = map f (filter positives xs)
    where
        f x         = x * x
        positives x = x > 0
-- map already uses the head, x, so can do it like above
-- filter also applies x, so don't need to define etc

oddLengthSums :: [[Int]] -> [Int]
oddLengthSums [] = []
oddLengthSums (x:xs)
    | odd (length x) = sum x : oddLengthSums xs
    | otherwise      = oddLengthSums xs

oddLengthSums' :: [[Int]] -> [Int]
oddLengthSums' xs = map sum (filter f xs)
    where
        f x = odd (length x)


------------------------- Exercise 3

-- remove :: [String] -> String -> [String]
-- remove [] _ = []
-- remove (x:xs) y
--     | x == y    = xs
--     | otherwise = x : remove xs y

-- removeAll :: [String] -> [String] -> [String]
-- removeAll xs []     = xs
-- removeAll xs (y:ys) = removeAll (remove xs y) ys

remove :: Eq a => [a] -> a -> [a]
remove xs y = filter f xs
    where
        f x = x /= y

-- Make without using other functions FIRST, ONLY USE OTHER FUNCTIONS IF IT SAYS

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll xs ys = filter f xs
    where
        f x = not (elem x ys)
-- elem returns a boolean if an element is present in a list

numbered :: [a] -> [(Int,a)]
-- numbered xs = zip f xs
--     where
--         f (length xs) =
numbered xs = zip [1..] xs

-- use [1..] to count up infinitely until length of other list

everyother :: [a] -> [a]
everyother xs = map snd (filter f (numbered xs))
    where
        f x = odd (fst x)
-- REMEMBER: filter, x can be any type. Can be pair, character, integer etc

same :: Eq a => [a] -> [a] -> [Int]
-- could've just done: same xs ys = map fst (filter snd (numbered (zipWith (==) xs ys))) - replace f with snd (if need to use more than one function than use f)
same xs ys = map fst (filter f (numbered (zipWith (==) xs ys)))
    where
        f x = snd x
-- To see what zipWith returns, look at the function used, == would of-course return a boolean
-- REMEMBER: zipWith returns a value for every comparison
