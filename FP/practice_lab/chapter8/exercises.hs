data Nat = Zero | Succ Nat
    deriving (Show)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

mult :: Nat -> Nat -> Nat
mult Zero m = Zero
mult (Succ n) m = add m $ mult n m

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
            (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node l y r) | x == y = True
                       | x < y = occurs' x l
                       | otherwise = occurs' x r

comp_occurs :: Ord a => a -> Tree a -> Bool
comp_occurs x (Leaf y) = (compare x y) == EQ
comp_occurs x (Node l y r) | compare x y == EQ = True
                           | compare x y == LT = comp_occurs x l
                           | otherwise = comp_occurs x r

data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)
    deriving Show

tb' :: Tree' Int
tb' = (Node' (Node' (Leaf' 1) (Leaf' 2))
            (Node' (Leaf' 3) (Leaf' 4)))

tnb' :: Tree' Int
tnb' = Node' (Leaf' 1) (Node' (Node' (Leaf' 1) (Leaf' 2))
        (Node' (Leaf' 3) (Leaf' 4)))

nrLeaves :: Tree' a -> Int
nrLeaves (Leaf' _) = 1
nrLeaves (Node' l r) = (nrLeaves l) + (nrLeaves r)

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = balanced l && balanced r &&
    abs (nrLeaves l - nrLeaves r) < 1

split_half :: [a] -> ([a], [a])
split_half as = (take h as, drop h as)
    where h = length as `div` 2

balance :: [a] -> Tree' a
balance [a] = Leaf' a
balance as = Node' (balance f) (balance s)
    where (f, s) = split_half as

data Expr = Val Int | Add Expr Expr
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val i) = f i
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)

eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

