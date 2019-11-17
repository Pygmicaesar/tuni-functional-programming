points2D :: Int -> [(Int,Int)]
points2D a = [(x,y) | x <- [(-a)..a], y <- [(-a)..a], abs x + abs y <= a]