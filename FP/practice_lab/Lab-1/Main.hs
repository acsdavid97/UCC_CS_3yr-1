{-
 - Name: Acs David
 - Number: 117106523
 - Assignment: 1
-}

-- the knight1 function just returns the bool argument
knight1 :: Bool -> Bool
knight1 q = q

-- the knave1 function returns the compliment of the passed argument
knave1 :: Bool -> Bool
knave1 q = not q

-- knight2 using pattern matching
knight2 :: Bool -> Bool
knight2 True = True
knight2 False = False

-- knave2 using a lambda expression
knave2 :: Bool -> Bool
knave2 = \q -> not q

-- knave3 using partial application
knave3 :: Bool -> Bool
knave3 = not

-- inhabitants function returning a list of knights and knaves
inhabitants :: [Bool -> Bool]
inhabitants = [knight1, knight2, knave1, knave2, knave3]

-- returns the i-th inhabitant from the list inhabitants using list indexing
getInhabitant :: Int -> Bool -> Bool
getInhabitant i = inhabitants !! i

-- returns the value that is returned if you ask the 
-- i-th inhabitant, with value b
-- first we use the getInhabitant function to get the i-th inhabitant 
-- and then apply b to that function
ask :: Int -> Bool -> Bool
ask i b = (getInhabitant i) b

-- double_negation is not equivalent to knave1 since it does not 
-- return the same values for all of the inputs
-- double_negation is equivalent to knight1 since it returns
-- True for True and False for False.
-- double_negation is implemented using composing the functions
-- knave1 and knave1 : knave1(knave1 b))
double_negation :: Bool -> Bool
double_negation b = (knave1.knave1) b

-- interrogate implemented using a function composition
-- if the inhabitant is a knave it will double negate, meaning
-- it will return the value of b
-- if the inhabitant is a knight it will "double return same value", which
-- will return the original argument
interrogate1 :: (Bool -> Bool) -> Bool -> Bool
interrogate1 inhabitant b = (inhabitant.inhabitant) b

-- interrogate implemented with a lambda expression
interrogate2 :: (Bool -> Bool) -> Bool -> Bool
interrogate2 inhabitant = \b -> (inhabitant.inhabitant) b

-- interrogate implemented with a partial application
interrogate3 :: (Bool -> Bool) -> Bool -> Bool
interrogate3 inhabitant = (inhabitant.inhabitant)

