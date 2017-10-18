 1   {-
   1  - Name: Acs David.
   2  - Number: 117106523.
   3  - Assignment: 02.
   4  -}
   5 
   6 paint_wall :: [[a]] -> [a] -> [a] -> [a] -> [a] -> [[a]] -> [[a]]
   7 -- we ran out of colours
   8 paint_wall painted_walls painted_wall [] original_colours wall walls =
   9     paint_wall painted_walls painted_wall original_colours original_colours wall walls
  10 -- at the end of the wall
  11 paint_wall painted_walls painted_wall colours original_colours [] walls =
  12     paint_walls_recursively (painted_wall:painted_walls) colours original_colours walls
  13 -- just a single brick
  14 paint_wall painted_walls painted_wall colours original_colours [brick] walls =
  15     paint_wall painted_walls (brick:painted_wall) colours original_colours [] walls
  16 -- append colour to the beginning of the wall
  17 paint_wall painted_walls painted_wall (colour:colours) original_colours (brick:bricks) walls =
  18     paint_wall painted_walls (colour:painted_wall) colours original_colours bricks walls
  19 
  20 paint_walls_recursively :: [[a]] -> [a] -> [a] -> [[a]] -> [[a]] 
  21 paint_walls_recursively painted_walls _ _ [] = painted_walls
  22 paint_walls_recursively painted_walls colours original_colours ((brick:bricks):walls) =
  23     paint_wall painted_walls [brick] colours original_colours bricks walls
  24 
  25 paint_interior_bricks :: [a] -> [[a]] -> [[a]]
  26 paint_interior_bricks colours walls = paint_walls_recursively [] colours colours walls 
  27 
  28 test1 :: IO ()
  29 test1 = putStrLn (show (paint_interior_bricks colours walls))
  30     where colours = [0, 1, 2]
  31           walls = [[3,3,3,3,3,3]]
  32 
  33 main :: IO ()
  34 main = putStrLn (show (paint_interior_bricks colours walls))
  35     where colours = [0, 1, 2]
  36           walls = [[3],[3,3],[3,3,3],[3,3,3],[3,3,3,3,3],[3,3,3,3,3,3]]
