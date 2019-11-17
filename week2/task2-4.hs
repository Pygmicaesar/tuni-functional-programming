commonChars :: String -> String -> String
commonChars [] _ = []
commonChars _ [] = []
commonChars (x:xs) (y:ys)
  | y `elem` (x:xs) = y : commonChars ys (drop (index y (x:xs)) xs)
  | x `elem` (y:ys) = x : commonChars xs (drop (index x (y:ys)) ys)
  | otherwise = commonChars xs ys
  where index :: Char -> String -> Int
        index _ [] = 0
        index c (x:xs)
          | c /= x = 1 + index c xs
          | otherwise = 0