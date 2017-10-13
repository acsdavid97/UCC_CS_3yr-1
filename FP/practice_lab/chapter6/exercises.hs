sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

expo :: Int -> Int -> Int
expo n 0 = 1
expo 0 e = 0
expo n e = n * expo n (e-1)

euclid :: Int -> Int -> Int
euclid a b  | a == b = a
            | otherwise = euclid (ma - mi) mi
                            where ma = max a b
                                  mi = min a b

andAll :: [Bool] -> Bool
andAll [] = True
andAll (b:bs) = b && andAll bs

concat' :: [[a]] -> [a]
concat' [] = []
concat' [as] = as
concat' (as:ass) = as ++ concat' ass

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a : replicate' (n-1) a

indexIt :: [a] -> Int -> a
indexIt (a:_) 0 = a
indexIt (a:as) n = indexIt as (n-1)

merge :: Ord a => [a] -> [a] -> [a]
merge [] bs = bs
merge as [] = as
merge (a:as) (b:bs) | a < b = a : merge as (b:bs)
                    | otherwise = b : merge (a:as) bs

halve :: [a] -> ([a], [a])
halve as = (take h as, drop h as)
            where h = length as `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort (a:as) = merge (msort first_half) (msort second_half)
                where first_half = fst halved
                      second_half = snd halved
                      halved = halve (a:as)

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (a:as) = a + sum as

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n [] = []
take' n (a:as) = a : take' (n - 1) as

last' :: [a] -> a
last' [a] = a
last' (a:as) = last' as
