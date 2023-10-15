

------------------------- Exercise 1

doubles :: [Int] -> [Int]
doubles xs = [(2*) x | x <- xs]

odds :: [Int] -> [Int]
odds xs = [x | x <- xs, odd x]

doubleodds :: [Int] -> [Int]
doubleodds xs = [(2*) x | x <- xs, odd x]

shorts :: [String] -> [String]
shorts xs = [x | x <- xs, length x <= 5]

squarePositives :: [Int] -> [Int]
squarePositives xs = [ x * x | x <- xs , 0 < x ]

oddLengthSums :: [[Int]] -> [Int]
oddLengthSums xss = [ sum xs | xs <- xss , odd (length xs) ]

remove :: Eq a => [a] -> a -> [a]
remove xs y = [ x | x <- xs , x /= y ]

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll xs ys = [ x | x <- xs , not (elem x ys) ]

everyother :: [a] -> [a]
everyother xs = [ x | (i,x) <- zip [1..] xs , odd i ]
-- everyother xs = [ x | x <- zip [1..] xs , odd (fst x) ]
-- NOTE: list comprehension, cannot use x (or maybe try with then realise can't do with)
-- CAN CAHNGE THE x after |
-- don't need 'snd' if have x and (i,x) - already using second

same :: Eq a => [a] -> [a] -> [Int]
same xs ys = [ i | (i,x,y) <- zip3 [1..] xs ys , x == y ]
-- same xs ys = map fst (filter f (numbered (zipWith (==) xs ys)))
--     where
--         f x = snd x
-- numbered and zipWith creates 2 differents zips, but can combine into zip3 (as the 2 different zips share an element)


------------------------- Exercise 2

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = [ (x,y) | x <- xs , y <- ys ]

selfpairs :: [a] -> [(a,a)]
selfpairs xs = [ (x,y) | (x,i) <- zip xs [0..] , y <- drop i xs ]
-- can use drop whilst counting up x (with i), to create all possible pairs

pyts :: Int -> [(Int,Int,Int)]
pyts n = [ (x,y,z) | x <- [1..n] , y <- [x..n] , z <- [y..n] , x*x + y*y == z*z]
-- REMEMBER CONDITIONS: 1..n, x..n, y..n because x<y<z REMEMBER YES ? COMPRENDO? ? ?

-- FOR LIST COMPREHENSION:
-- comparisons or conditions go in the commas
-- map functions go before the |
-- look at the output to see about list comprehension input stuff, parameters thingies: the stuff before |