lucky 7 = "You are lucky!"
lucky 6 = "Nearly lucky!"
-- lucky x = "Not lucky with value " ++ (show x)

lucky x
  | x > 0 && x < 10 = "sort of a little lucky"
  | x < 3 = "I don't know what to say"

myFirstList = [1,2,3]

myInfList = [1,3..]

doubleMe x = x + x
multiplyUs x y z = x*y*z

doubleSmall x = if x < 20 then 2*x else x

factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n - 1)

maximum' [] = error "Empty list"
maximum' [x] = x 
maximum' (x:xs) = if x > maxRest then x else maxRest
  where maxRest = maximum' xs

replicate' n _
  | n < 0 = error "Cannot replicate negative items"
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x 

take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

repeat' x = x : repeat' x