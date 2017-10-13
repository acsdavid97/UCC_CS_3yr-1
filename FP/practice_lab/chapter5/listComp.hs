--sum of 1^2 + 2^2 + ... + 100^2
squaresSum = sum [x^2 | x <- [1..100]]

-- grid
grid :: Int -> Int -> [(Int, Int)]
grid n m = [(x, y) | x <- [0..n], y <- [0..m]]

square n = [(x, y) | (x, y) <- grid n n, x /= y]

replicate' :: Int -> a -> [a]
replicate' n a = [a | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n],sum (factors x) - x == x]

xys = [(x, y) | x <- [1, 2], y <- [3, 4]]

xys' = concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct as bs = sum [ai * bi | (ai, bi) <- zip as bs]
