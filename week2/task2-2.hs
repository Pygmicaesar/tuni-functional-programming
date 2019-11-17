validateIBAN :: String -> Bool
validateIBAN (a:b:rest)
  | length (a:b:rest) == 18 && [a,b] == "FI" = calculateModulo (a:b:rest)
  | otherwise = False
  where calculateModulo :: String -> Bool
        calculateModulo 