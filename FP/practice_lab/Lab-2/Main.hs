20 {-
  19  - Name: Acs David.
  18  - Number: 117106523.
  17  - Assignment: 02.
  16  -}
  15 
  14 paint_wall :: [a] -> [a] -> [a] -> [a] -> [[a]] -> [[a]]
  13 -- we ran out of colours
  12 paint_wall painted_wall [] original_colours wall walls =
  11     paint_wall painted_wall original_colours original_colours wall walls
  10 -- at the end of the wall
   9 paint_wall painted_wall colours original_colours [] walls =
   8     paint_walls_recursively painted_wall colours original_colours walls
   7 -- just a single brick
   6 paint_wall painted_wall colours original_colours [brick] walls =
   5     paint_wall (brick:painted_wall) colours original_colours [] walls
   4 -- append colour to the beginning of the wall
   3 paint_wall painted_wall (colour:colours) original_colours (brick:bricks) walls =
   2     paint_wall (colour:painted_wall) colours original_colours bricks walls 
   1 
 21  paint_walls_recursively :: [a] -> [a] -> [a] -> [[a]] -> [[a]] 
   1 paint_walls_recursively painted_wall _ _ [] = [painted_wall]
   2 paint_walls_recursively painted_wall colours original_colours ((brick:bricks):walls) =
   3     painted_wall : paint_wall [brick] colours original_colours bricks walls
   4 
   5 paint_interior_bricks :: [a] -> [[a]] -> [[a]]
   6 paint_interior_bricks colours walls = paint_walls_recursively [] colours colours walls
   7 
   8 test1 :: IO ()
   9 test1 = putStrLn (show (paint_interior_bricks colours walls))
  10     where colours = [0, 1, 2]
  11           walls = [[1, 2, 3 ,4 ,5 ,6 ,7 ,8, 9,0]]
  12           
  13 main :: IO ()
  14 main = putStrLn (show (paint_interior_bricks colours walls))
  15     where colours = [0, 1, 2]
  16           walls = [[3],[3,3],[3,3,3],[3,3,3],[3,3,3,3,3],[3,3,3,3,3,3]]
 ~                                                                                                                  
 ~                                                                                                                  
 ~                                                                                                                  
 ~                                                                                                                  
 ~                                                                                                                  
 ~                                                                                                                  
 ~                                                                                                                  
 ~                     
