{-
 - Name: Acs David.
 - Number: 117106523.
 - Assignment: 02.
 -}
 
-- paints a wall and when finished calls the reversing function 
-- using mutual recursion, below you can read each case explanation
paint_wall :: [a] -> [a] -> [a] -> [a] -> [[a]] -> [[a]]
-- we ran out of colours, refill them.
paint_wall painted_wall [] original_colours wall walls =
    paint_wall painted_wall original_colours original_colours wall walls
-- at the end of the wall, time to reverse the painted_wall
paint_wall painted_wall colours original_colours [] walls =
    paint_wall_rev [] colours original_colours painted_wall walls
-- just a single brick
paint_wall painted_wall colours original_colours [brick] walls =
    paint_wall (brick:painted_wall) colours original_colours [] walls
-- append colour to the beginning of the wall, normal case.
paint_wall painted_wall (colour:colours) original_colours (brick:bricks) walls =
    paint_wall (colour:painted_wall) colours original_colours bricks walls

-- reverses the painted list and then calls the paint_walls_recursively function
-- which will continue painting the walls.
paint_wall_rev :: [a] -> [a] -> [a] -> [a] -> [[a]] -> [[a]]
paint_wall_rev rev_wall colours original_colours [] walls =
    paint_walls_recursively rev_wall colours original_colours walls
paint_wall_rev rev_wall colours original_colours (brick:bricks) walls =
    paint_wall_rev (brick:rev_wall) colours original_colours bricks walls

-- will paint each of the walls supplied in the last argument, using the colours
-- given in the colours argument cyclically. This is a helper function to handle
-- all the cases and to maintain the possiblility of starting with different 
-- colours than original_colours.
paint_walls_recursively :: [a] -> [a] -> [a] -> [[a]] -> [[a]]
-- empty lists are discarded, since they cannot be painted
paint_walls_recursively [] colours original_colours ((brick:bricks):walls) = 
    paint_wall [brick] colours original_colours bricks walls
-- if there are no more walls to paint, return with the last painted_wall
paint_walls_recursively painted_wall _ _ [] = [painted_wall]
-- paint the next wall and put the painted_wall to it's place
paint_walls_recursively painted_wall colours original_colours ((brick:bricks):walls) =
    painted_wall : paint_wall [brick] colours original_colours bricks walls
    
-- will paint the walls using the colours, according to the problem statement.
paint_interior_bricks :: [a] -> [[a]] -> [[a]]
paint_interior_bricks colours walls = paint_walls_recursively [] colours colours walls

main :: IO ()
main = putStrLn (show (paint_interior_bricks colours walls))
    where colours = [0, 1, 2]
          walls = [[3],[3,3],[3,3,3], [3,3,3],[3,3,3,3,3],[3,3,3,3,3,3]]

