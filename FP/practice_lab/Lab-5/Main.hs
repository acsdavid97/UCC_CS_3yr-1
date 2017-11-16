{-
 - Name: Acs David.
 - Number: 117106523.
 - Assignment: 05.
 -}

module Assignment where

-- PART 1

-- simple data type, has two values: One and Two.
-- it derives Eq, Ord, Show, Read in order to be able to compare values
-- and to print these values.
data TwoValued = One | Two
    deriving (Eq, Ord, Show, Read)

-- TwoValued is an instance of Bounded, the min is One, the max is Two
instance Bounded TwoValued where
    minBound = One
    maxBound = Two

-- PART 2

-- Defining a class with only one function next
class Ord a => Incrementable a where
    -- the function next increments the value it receives
    next :: a -> a    

-- TwoValued is an instance of Incrementable
instance Incrementable TwoValued where
--  The next of One is Two, the next of Two is undefined
    next One = Two
    next _ = undefined

-- PART 3

-- Testable is a class defining the check function
class (Incrementable a, Ord a) => Testable a where
--  from start to end verify if the predicate is True
    check :: (a -> Bool) -> a -> a -> Bool
    check p start end | start > end = True
                      | start == end = p start
                      | otherwise = p start && check p (next start) end

-- TwoValued is an instance of Testable, meaning we can use check on it
instance Testable TwoValued

-- test check on TwoValued elements
ot = [One, Two]
test_check1 :: Bool
test_check1 = [True, False, True, False] ==
    [check (==One) first last | first <- ot, last <- ot]

test_check2 :: Bool
test_check2 = [False, False, True, True] ==
    [check (==Two) first last | first <- ot, last <- ot]

-- PART 4

-- A list of a is Incrementable in the context that the
-- elements are Incrementable, comparable and bounded.
instance (Incrementable a, Eq a, Ord a, Bounded a) => 
                                    Incrementable [a] where

--  the definition of next for lists: we will be able to increment a list
--  by incrementing the least significant one first and when we cannot 
--  increment the least significant one, we will start incrementing a
--  more significant elements. Just like adding one to number, if we 
--  treat the digits as distinct elements.
--  example: next [One, One] -> [One, Two]
--           next [One, Two] -> [Two, One]
    next = reverse . next' . reverse 
        where 
            next' :: (Incrementable a, Bounded a) => [a] -> [a]
            next' [] = []
            next' (a:as) | a == maxBound = minBound:next' as
            next' (a:as) | otherwise = next a:as

-- test next on list of TwoValued elements
test_next1 :: Bool
test_next1 = [Two, One] == next [One, Two]

test_next2 :: Bool
test_next2 = [One, Two] == next [One, One]

-- PART 5

-- if elements of a list are Incrementable, comparable, bounded
-- then we can test them using the function check, since the list 
-- itself is Incrementable.
instance (Incrementable a, Ord a, Bounded a) => Testable [a]

main :: IO()
main = do putStrLn $ show $ test ([One, Two, Two] ==)
          putStrLn $ show $ test ([One, Two, Two] /=)
          putStrLn $ show $ test ([One, Two, Two]  <)
          putStrLn $ show $ test ([Two, Two, Two] >=)
          putStrLn $ show $ test (>= [Two, Two, Two])
     where test p = [int p bs [Two, Two, Two] | bs <- bss]
           int p a a' = if check p a a' then 1 else 0
           bs = [One, Two]
           bss = [[a, b, c] | a <- bs, b <- bs, c <- bs]

