
data IntTree = Empty | Node Int IntTree IntTree
  deriving Show

t :: IntTree
t = Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 5 Empty (Node 6 Empty Empty))


------------------------- Exercise 1

isEmpty :: IntTree -> Bool
isEmpty Empty = True
isEmpty _     = False

rootValue :: IntTree -> Int
rootValue Empty        = 0
rootValue (Node i _ _) = i

height :: IntTree -> Int
height Empty        = 0
height (Node _ l r) = 1 + max (height l) (height r)
-- If recursively go down one branch, will return an integer or value. So in this example, max eventually gets the max height of each subtree

member :: Int -> IntTree -> Bool
member n Empty = False
member n (Node x l r)
    | x == n    = True
    | otherwise = member n l || member n r
-- REMEMBER: can use conditions and functions for recurisvely going down tree as will eventually give a value

paths :: Int -> IntTree -> [[Int]]
paths y Empty = []
paths y (Node x l r)
  | y == x    = [[]] ++ map (0 :) (paths y l) ++ map (1 :) (paths y r)
  | otherwise =         map (0 :) (paths y l) ++ map (1 :) (paths y r)
-- Use '++' when want to add separate lists and also in a situation where you are ADDING MINI-LISTS IN A BIGGER LIST

-- REMEMBER:
-- with trees, recursively calling function such as: member n l or paths y l as well as member n r or paths y r, IT WILL COVER (COVERY) EVERY CASE, EVERY NODE, ETC

-------------------------

{-
instance Show IntTree where
    show = unlines . aux ' ' ' '
      where
        aux _ _ Empty = []
        aux c d (Node x s t) = 
          [ c:' ':m | m <- aux ' ' '|' s ] ++ 
          ['+':'-':show x] ++ 
          [ d:' ':n | n <- aux '|' ' ' t ]
-}



------------------------- Exercise 2

type Var = ()

data Term
--  deriving Show

{-
example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Apply (Variable "a") (Variable "c"))) (Variable "x")) (Variable "b")))
-}

{-
pretty :: Term -> String
pretty = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m 
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m
-}

n1 :: Term
n1 = undefined

n2 :: Term
n2 = undefined

n3 :: Term
n3 = undefined

used :: Term -> [Var]
used = undefined

free :: Term -> [Var]
free = undefined



-------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

minus :: Ord a => [a] -> [a] -> [a]
minus xs [] = xs
minus [] ys = []
minus (x:xs) (y:ys)
    | x <  y    = x : minus    xs (y:ys)
    | x == y    =     minus    xs    ys
    | otherwise =     minus (x:xs)   ys
