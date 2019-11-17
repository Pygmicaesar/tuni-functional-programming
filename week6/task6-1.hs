-- let's imagine these are all valid country codes
codes = ["358", "359", "41", "84"]

-- define some types
data PhoneType = WorkLandLine | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read)
newtype CountryCode = CountryCode Integer deriving (Eq, Read) 
newtype PhoneNo = PhoneNo Integer deriving (Eq, Read)

-- define Phone with record syntax
data Phone = Phone { phoneType :: Maybe PhoneType
                   , countryCode :: Maybe CountryCode
                   , phoneNo :: PhoneNo
                   } deriving (Eq, Read)

-- Show instances for CountryCode and PhoneNo
instance Show CountryCode where
  show (CountryCode code) = '+' : show code
instance Show PhoneNo where
  show (PhoneNo number) = show number

-- Show instance for Phone
instance Show Phone where
  show (Phone Nothing Nothing pn)     = show pn
  show (Phone (Just pt) Nothing pn)   = show pn ++ " (" ++ show pt ++ ")"
  show (Phone Nothing (Just cc) pn)   = show cc ++ " " ++ show pn
  show (Phone (Just pt) (Just cc) pn) = show cc ++ " " ++ show pn ++ " (" ++ show pt ++ ")"

-- function that creates a country code
createCountryCode :: Integer -> CountryCode
createCountryCode code
  | code < 0    = error "The country code can't be negative!"
  | not $ (show code) `elem` codes = error "The country code is not valid!"
  | otherwise   = CountryCode code

-- function that creates a phone number
createPhoneNo :: Integer -> PhoneNo
createPhoneNo number
  | number < 0  = error "The phone number can't be negative!"
  | otherwise   = PhoneNo number

-- function that creates a phone
phone :: Maybe PhoneType -> Maybe CountryCode -> PhoneNo -> Phone
phone pt cc pn = Phone pt cc pn

readMaybe :: (Read a) => String -> Maybe a
readMaybe st =
  case reads st of
    [(x,"")] -> Just x
    _ -> Nothing

readPhone :: String -> String -> String -> Phone
readPhone pt ('+':xs) pn      = readPhone pt xs pn
readPhone pt ('0':'0':xs) pn  = readPhone pt xs pn
readPhone pt cc pn =
  let pt' = readMaybe pt
      cc' = case readMaybe cc :: Maybe Integer of
              Nothing -> Nothing
              Just x -> Just (createCountryCode x)
      pn' = createPhoneNo $ read pn
  in phone pt' cc' pn'
