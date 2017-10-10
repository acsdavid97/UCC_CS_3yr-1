halve :: [a] -> ([a], [a])
halve as = (take half as, drop half as)
            where half = length as `div` 2

third :: [a] -> a

-- head and tail
-- third as = head (tail (tail as))

-- list indexing
-- third as = as !! 2

-- pattern matching
third [a, b, c, _] = c


safetail :: [a] -> [a]

-- conditional expression
-- safetail as = if null as then [] else tail as

-- guarded expressions
-- safetail as | null as = []
--             | otherwise = tail as

-- pattern matching
safetail [] = []
safetail as = tail as

-- mult :: Int -> Int -> Int -> Int
-- mult x = \y -> \z -> x * y * z

-- Luhn algorithm
luhnDouble :: Int -> Int
luhnDouble d | double > 9 = double - 9
             | otherwise = double
             where double = 2 * d

luhn a b c d = (luhnDouble a + luhnDouble b + luhnDouble c + luhnDouble d) `mod` 10 == 0
