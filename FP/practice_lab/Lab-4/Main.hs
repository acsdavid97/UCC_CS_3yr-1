{-
 - Name: Acs David.
 - Number: 117106523.
 - Assignment: 04.
 -}

module Main where

-- polymorphic, recursive data type List, 
-- can be and EmptyList or an element and a List. 
-- Constructor is Cons. if the list is not empty it consists of 
-- the first element and the rest of the elements.
data List a = EmptyList |
              Cons { first :: a
                   , rest :: List a
              }

-- pretty prints a list, if the elements of the list are "showable"
pretty_print :: (Show a) => List a -> String
pretty_print as = "[" ++ pretty_print_rec as

-- recursively prints the elements of a list
-- if the list is empty just a right square bracket is returned to
-- complement the left square bracket added previously
-- if there is a single element just the element is printed 
-- (without a comma), in order to avoid superfluous commas
-- if there are more elements in the list then an element and a
-- comma is added to the string version of the list
pretty_print_rec :: (Show a) => List a -> String
pretty_print_rec EmptyList = "]" 
pretty_print_rec (Cons {first=a, rest=EmptyList}) = show a ++ "]" 
pretty_print_rec (Cons {first=a, rest=as}) = 
            show a ++ "," ++ pretty_print_rec as

-- Constructs an ordered List from a normal Haskell list([]).
-- each of the elements is copied from the Haskell list and
-- inserted in order into the user defined List.
create_from_list :: (Ord a) => [a] -> List a
create_from_list = foldr (flip insert) EmptyList

-- insert an element in order, into an ordered list
-- The function assumes that the list is ordered.
insert :: (Ord a) => List a -> a -> List a
insert EmptyList i = Cons {first=i, rest=EmptyList}
insert l@(Cons {first=a, rest=as}) i | i <= a = Cons {first=i, rest=l}
                                     | otherwise = 
                                         Cons {first=a, rest=(insert as i)}

-- equivalent of foldl for Haskell lists.
-- use an accumulator and forward recursion, apply i as first element
foldl' :: (b -> a -> b) -> b -> List a -> b
foldl' f i EmptyList = i
foldl' f i (Cons {first=a, rest=as}) = foldl' f (f i a) as

-- equivalent of foldr for Haskell lists.
-- backward recursion and apply i as last element
foldr' :: (a -> b -> b) -> b -> List a -> b
foldr' f i EmptyList = i
foldr' f i (Cons {first=a, rest=as}) = f a (foldr' f i as)

-- tests create_from_list and pretty_print
main :: IO()
main = putStrLn $ pretty_print $ create_from_list list
    where list = [1,2,3,5,4] :: [Int]

