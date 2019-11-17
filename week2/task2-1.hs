digits = ['0'..'9']

checkDigits :: [Char] -> Bool
checkDigits [] = False
checkDigits [x]
  | elem x digits = True
  | otherwise = False
checkDigits (x:xs)
  | elem x digits = checkDigits xs
  | otherwise = False