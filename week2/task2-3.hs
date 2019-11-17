calculatePairs :: (Char, Char) -> Int -> String -> Int
calculatePairs _ _ [x] = 0
calculatePairs (a,b) g (x:xs)
  | length xs == g = 0
  | x == a && xs !! g == b = 1 + calculatePairs (a,b) g xs
  | otherwise = calculatePairs (a,b) g xs