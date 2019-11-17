firstAndLast :: [String] -> Char -> [String]
firstAndLast [] _ = error "Empty list!"
firstAndLast s c = [x | x <- s, head x == c || last x == c]