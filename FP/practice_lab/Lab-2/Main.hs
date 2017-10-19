                                                                                      
{-
 - Name: Acs David.
 - Number: 117106523.
 - Assignment: 02.
 -}
 
paint_wall :: [a] -> [a] -> [a] -> [a] -> [[a]] -> [[a]]
-- we ran out of colours
paint_wall painted_wall [] original_colours wall walls =
    paint_wall painted_wall original_colours original_colours wall walls
-- at the end of the wall
paint_wall painted_wall colours original_colours [] walls =
    paint_wall_rev [] colours original_colours painted_wall walls
-- just a single brick
paint_wall painted_wall colours original_colours [brick] walls =
    paint_wall (brick:painted_wall) colours original_colours [] walls
-- append colour to the beginning of the wall
paint_wall painted_wall (colour:colours) original_colours (brick:bricks) walls =
    paint_wall (colour:painted_wall) colours original_colours bricks walls

paint_wall_rev :: [a] -> [a] -> [a] -> [a] -> [[a]] -> [[a]]
paint_wall_rev rev_wall colours original_colours [] walls =
    paint_walls_recursively rev_wall colours original_colours walls
paint_wall_rev rev_wall colours original_colours (brick:bricks) walls =
    paint_wall_rev (brick:rev_wall) colours original_colours bricks walls

paint_walls_recursively :: [a] -> [a] -> [a] -> [[a]] -> [[a]]
paint_walls_recursively [] colours original_colours ((brick:bricks):walls) = 
    paint_wall [brick] colours original_colours bricks walls
paint_walls_recursively painted_wall _ _ [] = [painted_wall]
paint_walls_recursively painted_wall colours original_colours ((brick:bricks):walls) =
    painted_wall : paint_wall [brick] colours original_colours bricks walls
    
paint_interior_bricks :: [a] -> [[a]] -> [[a]]
paint_interior_bricks colours walls = paint_walls_recursively [] colours colours walls
 
test1 :: IO ()
test1 = putStrLn (show (paint_interior_bricks colours walls))
    where colours = [0, 1, 2]
          walls = [[89, 2, 3 ,4 ,5 ,6 ,7 ,8, 9, 99]]

main :: IO ()
main = putStrLn (show (paint_interior_bricks colours walls))
    where colours = [0, 1, 2]
          walls = [[3],[3,3],[3,3,3], [3,3,3],[3,3,3,3,3],[3,3,3,3,3,3]]

