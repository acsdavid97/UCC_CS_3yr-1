module Example where

data Op = Add | Sub | Mul | Div
    deriving (Read)

instance Show Op where
    show op = case op of
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"

-- check if an operation and two integers produce a corect result
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub l r = l > r
valid Mul _ _ = True
valid Div l r = l `mod` r == 0

test1 :: Bool
test1 = valid Sub 2 1

test2 :: Bool
test2 = not $ valid Div 3 2

apply :: Op -> Int -> Int -> Int
apply op l r = case op of
    Add -> l + r
    Sub -> l - r
    Mul -> l * r
    Div -> l `div` r

test3 :: Bool
test3 = 3 == apply Add 2 1

test4 :: Bool
test4 = 0 /= apply Add 123 321

data Expr = Val Int
          | App Op Expr Expr

valid_test_expression :: Expr
valid_test_expression = App Mul (Val 3) (App Add (Val 2) (Val 1))

invalid_test_expression :: Expr
invalid_test_expression = App Sub (Val 3) (App Add (Val 2) (Val 1))

instance Show Expr where
    show (Val v) = show v
    show (App op l r) = "(" ++ show l ++ show op ++ show r ++ ")"

test5 :: Bool
test5 = show valid_test_expression == "(3*(2+1))"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

test6 :: Bool
test6 = values valid_test_expression == [3, 2, 1]

test7 :: Bool
test7 = values (Val 3) == [3]

mother_of_all_tests :: Bool
mother_of_all_tests = and [test1, test2, test3, test4, test5, test6, test7]

eval :: Expr -> [Int]
eval (Val v) = [v | v > 0]
eval (App o l r) = [apply o vl vr | vl <- eval l,
                                    vr <- eval r,
                                    valid o vl vr]

test8 :: Bool
test8 = [9] == eval valid_test_expression

test9 :: Bool
test9 = [] == eval invalid_test_expression

-- [0,1,2] -> [[0,1,2],[0,1],[0,2],[1,2],[0],[1],[2],[]]
--      starting with 0:
--      [0, 1, 2]
--      [0, 1],[0, 2]
--      [0]
--      starting with 1:
--      [1, 2], [1]
--      starting with 2:
--      [2]
--      starting with nothing
--      []
subs :: [a] -> [[a]]
subs [] = [[]]
subs (a:as) = ss ++ map (a:) ss
    where ss = subs as

-- interleave 1 [2, 3, 4] -> [[1, 2, 3, 4], [2, 1, 3, 4], [2, 3, 1, 4], [2, 3, 4, 1]]
interleave :: a -> [a] -> [[a]]
interleave i []     = [[i]]
interleave i (a:as) = ((i:a:as):(map (a:) $ interleave i as)) 
-- interleave i (a:as) = ((i:a:as):[a:is | is <- interleave i as])

-- perms [1, 2, 3] -> [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]
perms :: [a] -> [[a]]
perms []     = [[]]
perms (a:as) = concat $ map (interleave a) $ perms as
--             concat [interleave a ps | ps <- perms as]

choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- Sections 9.5
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

test10 :: Bool
test10  = solution valid_test_expression [1, 2, 3] 9
test11 :: Bool
test11 = not $ solution valid_test_expression [1, 2, 3] 10
test12 :: Bool
test12 = not $ solution valid_test_expression [4, 2, 3] 10
