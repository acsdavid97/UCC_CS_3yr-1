{-
 - Name: Acs David.
 - Number: 117106523.
 - Assignment: 05.
 -}

module Assignment where

-- PART 1

data TwoValued = One | Two
    deriving (Eq, Ord, Show, Read)

instance Bounded TwoValued where
    minBound = One
    maxBound = Two

-- PART 2

class Ord a => Incrementable a where
    next :: a -> a    

instance Incrementable TwoValued where
    next One = Two
    next _ = undefined

-- PART 3

class (Incrementable a, Ord a) => Testable a where
    check :: (a -> Bool) -> a -> a -> Bool

    check p start end | start > end = True
                      | start == end = p start
                      | otherwise = p start && check p (next start) end

instance Testable TwoValued

-- PART 4

instance (Incrementable a, Eq a, Ord a, Bounded a) => Incrementable [a] where

    next = reverse . next' . reverse 
        where 
            next' :: (Incrementable a, Bounded a) => [a] -> [a]
            next' [] = []
            next' (a:as) | a == maxBound = minBound:next' as
            next' (a:as) | otherwise = next a:as

-- PART 5

instance Testable [TwoValued]

main = do putStrLn $ show $ test ([One, Two, Two] ==)
          putStrLn $ show $ test ([One, Two, Two] /=)
          putStrLn $ show $ test ([One, Two, Two]  <)
          putStrLn $ show $ test ([Two, Two, Two] >=)
          putStrLn $ show $ test (>= [One, Two, Two])
    where 
        test p = [int p bs [Two, Two, Two] | bs <- bss]
        int p a a' = if check p a a' then 1 else 0
        bs = [One, Two]
        bss = [[a, b, c] | a <- bs, b <- bs, c <- bs]
