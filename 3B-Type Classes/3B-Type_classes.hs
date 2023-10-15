

ladies    = ["Mary","Kitty","Lydia","Elizabeth","Jane"]
gentlemen = ["Charles","Fitzwilliam","George","William"]
couples   = [("Elizabeth","Fitzwilliam"),("Charlotte","William"),("Lydia","George"),("Jane","Charles")]

------------------------- Exercise 1

-- [a] means list of any type
-- NOTE: make sure to check for case of empty something, like empty integer (0) or empty list ([])
ditch :: Int -> [a] -> [a]
ditch _ [] = []
ditch 0 xs = xs
ditch n (x:xs) = ditch (n-1) xs

at :: [a] -> Int -> a
at (x:xs) 0 = x
at (x:xs) n = at xs (n-1)
-- good practice to have _ instead of x:xs if not using xs or x


------------------------- Exercise 2

find :: Eq a => a -> [(a,b)] -> b
find x [] = error "find: not found"
find x ((y,z):zs)
    | x == y    = z
    | otherwise = find x zs

which :: Eq a => a -> [a] -> Int
which = aux 0
  where
    aux :: Eq a => Int -> a -> [a] -> Int
    aux i n [] = error "which: not found"
    aux i n (x:xs)
        | n == x    = i
        | otherwise = aux (i+1) n xs

member :: Eq a => [a] -> a -> Bool
member    []  _ = False
member (x:xs) y
    | x == y    = True
    | otherwise = member xs y

remove :: Eq a => [a] -> a -> [a]
remove [] _ = []
remove (x:xs) y
    | x == y    = xs
    | otherwise = x : remove xs y

-- Uses <, > etc so use Ord
before :: Ord a => [a] -> [a] -> Bool
before _ [] = False
before [] _ = True
before (x:xs) (y:ys)
    | x == y    = before xs ys
    | x < y     = True
    | otherwise = False

sorted :: Ord a => [a] -> Bool
sorted []       = True
sorted [x]      = True
sorted (x:y:ys) = (x < y) && (sorted (y:ys))

------------------------- Exercise 3

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)
    | x == y    = x : merge xs ys
    | otherwise = y : merge (x:xs) ys
-- x == y and choose one element because it is a non-repeating list

minus :: Ord a => [a] -> [a] -> [a]
-- minus [] (_:_)  = []
-- minus (_:xs) [] = xs
minus xs [] = xs
minus [] ys = ys
minus (x:xs) (y:ys)
    | x == y    = minus xs (y:ys)
    | otherwise = x : minus xs (y:ys)

-- msort :: Ord a => [a] -> [a]
-- msort [] = []

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort (take n xs)) (msort (drop n xs))
  where
    n = div (length xs) 2
-- use 'take' to get more than one item of a list (and drop to get the rest)

msort' :: Ord a => [a] -> [a]
msort' []  = []
msort' [x] = [x]
msort' xs  = msort' ys `merge` msort' zs
  where
    (ys,zs) = splitAt (length xs `div` 2) xs

msort'' :: Ord a => [a] -> [a]
msort'' []  = []
msort'' [x] = [x]
msort'' xs  = msort'' ys `merge` msort'' zs
  where
    (ys,zs) = f ([],[]) xs
    f p [] = p
    f (ys,zs) [x] = (x:ys,zs)
    f (ys,zs) (z:y:xs) = f (y:ys,z:zs) xs