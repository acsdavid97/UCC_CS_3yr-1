{-
 - Name: Acs David.
 - Number: 117106523.
 - Assignment: 02.
 -}

paint_interior_of_wall :: [a] -> [a] -> [a] -> ([a], [a])
paint_interior_of_wall colours _ [] = (colours, [])
paint_interior_of_wall colours _ [a] = (colours, [a])
paint_interior_of_wall [] orignal_colours wall = 
    paint_interior_of_wall orignal_colours orignal_colours wall
paint_interior_of_wall (colour:colours) orignal_colours (brick:bricks) =
    (remaining_colours, colour : painted_bricks)
        where (remaining_colours, painted_bricks) = paint_interior_of_wall colours orignal_colours bricks

paint_except_first_brick :: [a] -> [a] -> [a] -> ([a], [a])
paint_except_first_brick colours orignal_colours (brick:bricks) = 
    (remaining_colours, brick : painted_bricks)
        where (remaining_colours, painted_bricks) = 
                paint_interior_of_wall colours orignal_colours bricks

paint_interior_bricks_recursive :: [a] -> [a] -> [[a]] -> ([a], [[a]])
paint_interior_bricks_recursive colours _ [] = (colours, [])
paint_interior_bricks_recursive colours orignal_colours (wall:walls) =
    (remaining_colours, painted_wall:painted_walls)
        where (remaining_colours, painted_wall) = 
                paint_except_first_brick colours orignal_colours wall
              (_, painted_walls) = 
                paint_interior_bricks_recursive remaining_colours orignal_colours walls

-- paints the interior of each wall, which are supplied as a list
-- in the second argument to the function, using the colours cyclically 
-- given in the first argument of the function. 
paint_interior_bricks :: [a] -> [[a]] -> [[a]]
paint_interior_bricks colours walls = painted_walls
    where (_, painted_walls) = 
            paint_interior_bricks_recursive colours colours walls

test1 :: IO ()
test1 = putStrLn (show (paint_interior_bricks colours walls))
    where colours = [0, 1, 2]
          walls = [[3, 3, 3],[3,3],[3,3,3],[3,3,3],[3,3,3,3,3,3],[3,3,3,3,3,3]]

main :: IO ()
main = putStrLn (show (paint_interior_bricks colours walls))
    where colours = [0, 1, 2]
          walls = [[3],[3,3],[3,3,3],[3,3,3],[3,3,3,3,3],[3,3,3,3,3,3]]
