{-
 - Name: Acs David.
 - Number: 117106523.
 - Assignment: 02.
 -}

paint_interior_of_wall :: [a] -> [a] -> [a] -> [a]
paint_interior_of_wall _ _ [] = []
paint_interior_of_wall _ _ [a] = [a]
paint_interior_of_wall [] orignal_colours wall = 
    paint_interior_of_wall orignal_colours orignal_colours wall
paint_interior_of_wall (colour:colours) orignal_colours (brick:bricks) =
    colour : paint_interior_of_wall colours orignal_colours bricks

paint_except_first_brick :: [a] -> [a] -> [a]
paint_except_first_brick colours (brick:bricks) = 
    brick : paint_interior_of_wall colours colours bricks

-- paints the interior of each wall, which are supplied as a list
-- in the second argument to the function, using the colours cyclically 
-- given in the first argument of the function. 
paint_interior_bricks :: [a] -> [[a]] -> [[a]]
paint_interior_bricks colours [] = []
paint_interior_bricks colours (wall:walls) = 
    paint_except_first_brick colours wall : paint_interior_bricks colours walls

main :: IO ()
main = putStrLn (show (paint_interior_bricks colours walls))
    where colours = [0, 1, 2]
          walls = [[3],[3,3],[3,3,3],[3,3,3],[3,3,3,3,3],[3,3,3,3,3,3]]
