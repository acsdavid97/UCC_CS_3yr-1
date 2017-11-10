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
