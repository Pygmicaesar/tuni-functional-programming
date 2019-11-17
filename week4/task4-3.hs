-- let's imagine these are all valid country codes
codes = [1..999]

-- define some types
data PhoneType = WorkLandLine | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read)
data CountryCode = CountryCode Integer deriving (Eq) 
data PhoneNo = PhoneNo Integer deriving (Eq)

-- define Phone with record syntax
data Phone = Phone { phoneType :: PhoneType
                   , countryCode :: CountryCode
                   , phoneNo :: PhoneNo
                   } deriving (Eq)

-- Show instances for CountryCode and PhoneNo
instance Show CountryCode where
  show (CountryCode code) = "+" ++ show code
instance Show PhoneNo where
  show (PhoneNo number) = show number

-- Show instance for Phone
instance Show Phone where
  show (Phone pt cc pn) = show cc ++ " " ++ show pn ++ " " ++ "(" ++ show pt ++ ")" 

-- function that creates a country code
createCountryCode :: Integer -> CountryCode
createCountryCode code
  | code < 0    = error "The country code can't be negative!"
  | otherwise   = CountryCode code

-- function that creates a phone number
createPhoneNo :: Integer -> PhoneNo
createPhoneNo number
  | number < 0  = error "The phone number can't be negative!"
  | otherwise   = PhoneNo number

-- function that creates a phone
phone :: PhoneType -> CountryCode -> PhoneNo -> Phone
phone pt cc pn = Phone { phoneType = pt
                       , countryCode = cc
                       , phoneNo = pn
                       }

readPhone :: String -> String -> String -> Phone
readPhone pt ('+':xs) pn      = readPhone pt xs pn
readPhone pt ('0':'0':xs) pn  = readPhone pt xs pn
readPhone pt cc pn
  | not (elem (read cc :: Integer) codes) = error "The country code is not valid!"
  | otherwise = phone (read pt :: PhoneType) (createCountryCode (read cc :: Integer)) (createPhoneNo (read pn :: Integer))