-- [f x | x <- xs, p x]

filter_and_map :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filter_and_map f p as = map f (filter p as)

filter_and_map' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filter_and_map' f p = map f . filter p

all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr ((&&) . p) True

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr ((||) . p) False

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (a:as) | p a = a:takeWhile' p as
                    | otherwise = []

-- takeWhile' p =  foldr (\x acc -> if p x then x:acc else []) []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (a:as) | p a = dropWhile' p as
                    | otherwise = (a:as)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x:acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x:acc else acc) []

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f p = f (fst p) (snd p)
