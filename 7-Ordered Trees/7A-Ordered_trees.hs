
data IntTree = Empty | Node Int IntTree IntTree
-- data Tree a = Leaf | Branch a (Tree a) (Tree a)

t :: IntTree
t = Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 5 Empty (Node 6 Empty Empty))

instance Show IntTree where
    show = unlines . aux ' ' ' '
      where
        aux _ _ Empty = []
        aux c d (Node x s t) = 
          [ c:' ':m | m <- aux ' ' '|' s ] ++ 
          ['+':'-':show x] ++ 
          [ d:' ':n | n <- aux '|' ' ' t ]

isEmpty :: IntTree -> Bool
isEmpty Empty = True
isEmpty _     = False

------------------------- Exercise 1

member :: Int -> IntTree -> Bool
member n Empty = False
member n (Node x l r)
    | n == x = True
    | n < x  = member n l
    | n > x  = member n r
--  | otherwise = member n r
-- last case should really be otherwise

largest :: IntTree -> Int
largest Empty            = error "No largest value in tree"
largest (Node x l Empty) = x
largest (Node x l r)     = largest r

smallest :: IntTree -> Int
smallest = undefined

ordered :: IntTree -> Bool
ordered Empty = True
ordered (Node x l r) = (not (isEmpty l) || (largest l < x && ordered l)) && (not (isEmpty r) || (x > smallest r && ordered r))
-- CHECK IF LARGEST OF LEFT-HAND SIDE IS LESS THAN X, then also check AND condition with recursively calling function

deleteLargest :: IntTree -> IntTree
deleteLargest Empty            = error "No largest value in tree"
deleteLargest (Node x l Empty) = l
deleteLargest (Node x l r)     = (Node x l (deleteLargest r))
-- RETURNS left, l, because since r is empty (largest node), it just replaces it with whatever is on the left

delete :: Int -> IntTree -> IntTree
delete _ Empty = Empty
-- Return original tree, just output Empty
delete y (Node x l r)
    | y < x     = (Node x (delete y l) r)
    | y > x     = (Node x l (delete y r))
    | isEmpty l = r
    | otherwise = (Node (largest l) (deleteLargest l) r)

------------------------- Exercise 2

{-
instance Show a => Show (Tree a) where
    show = unlines . aux ' ' ' '
      where
        aux _ _ Empty = []
        aux c d (Node x s t) = 
          [ c:' ':m | m <- aux ' ' '|' s ] ++ 
          ['+':'-':show x] ++ 
          [ d:' ':n | n <- aux '|' ' ' t ]
-}



------------------------- Lambda-calculus

type Var = String

data Term =
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term

example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Apply (Variable "a") (Variable "c"))) (Variable "x")) (Variable "b")))

pretty :: Term -> String
pretty = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m 
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m

instance Show Term where
  show = pretty

n1 :: Term
n1 = Lambda "x" (Variable "x")

n2 :: Term
n2 = Lambda "x" (Apply (Lambda "y" (Variable "x")) (Variable "z"))

n3 :: Term
n3 = Apply (Lambda "x" (Lambda "y" (Apply (Variable "x") (Variable "y")))) n1

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

-------------------------

used :: Term -> [Var]
used (Variable x) = [x]
used (Lambda x n) = [x] `merge` used n
used (Apply  n m) = used n `merge` used m

free :: Term -> [Var]
free (Variable x) = [x]
free (Lambda x n) = free n `minus` [x]
free (Apply  n m) = free n `merge` free m


------------------------- Exercise 3

numeral :: Int -> Term
numeral = undefined

------------------------- Exercise 4

variables :: [Var]
variables = undefined

fresh :: [Var] -> Var
fresh = undefined

rename :: Var -> Var -> Term -> Term
rename x y (Variable z)
    | z == x    = undefined
    | otherwise = undefined
rename x y (Lambda z n)
    | z == x    = undefined
    | otherwise = undefined
rename x y (Apply n m) = undefined

substitute :: Var -> Term -> Term -> Term
substitute = undefined
