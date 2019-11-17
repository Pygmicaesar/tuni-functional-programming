followedByGreater :: [Int] -> [Int]
followedByGreater [] = []
followedByGreater [x] = []
followedByGreater (x:xs)
  | x < head xs = x : followedByGreater xs
  | otherwise = followedByGreater xs