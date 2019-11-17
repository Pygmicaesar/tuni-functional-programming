-- let's imagine these are all valid country codes
codes = ["358", "359", "41", "84"]

-- define some types
data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read)
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
createPhone :: Maybe PhoneType -> Maybe CountryCode -> PhoneNo -> Phone
createPhone pt cc pn = Phone pt cc pn

-- readMaybe from the examples
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
  in createPhone pt' cc' pn'

-- PHONEBOOK IMPLEMENTATION --

type PhoneBookEntry = (String,[Phone])
data PhoneBook = EmptyPhoneBook | Node { entry :: PhoneBookEntry
                                       , leftBook :: PhoneBook
                                       , rightBook :: PhoneBook
                                       } deriving (Show, Read, Eq)

singleton :: PhoneBookEntry -> PhoneBook
singleton a = Node a EmptyPhoneBook EmptyPhoneBook

findEntries :: String -> PhoneBook -> PhoneBookEntry
findEntries x EmptyPhoneBook = ("",[])
findEntries query (Node (name,numbers) left right)
  | query == name = (name,numbers)
  | query < name  = findEntries query left
  | query > name  = findEntries query right

addEntry :: String -> String -> String -> String -> PhoneBook -> PhoneBook
addEntry nm pt cc pn book =
  let new = (nm,[readPhone pt cc pn])
  in insertTree new book
    where insertTree :: PhoneBookEntry -> PhoneBook -> PhoneBook
          insertTree a EmptyPhoneBook = singleton a
          insertTree a@(newName,newNumbers) (Node b@(name,numbers) left right) =
            case compare newName name of
              EQ -> 
                Node (name,(numbers ++ newNumbers)) left right
              LT -> 
                Node b (insertTree a left) right
              GT -> 
                Node b left (insertTree a right)

-- TESTING --
examplePhoneBook =
  addEntry "PersonA" "WorkLandline"  "00358"  "123456789"
  $ addEntry "PersonA" "PrivateMobile" "358"    "123456789"
  $ addEntry "PersonB" "Other"         "+358"   "123456789"
  $ addEntry "PersonB" "PrivateMobile" "358"    "123456789"
  $ addEntry "PersonA" "WorkLandline"  "00358"  "123456789"
  $ addEntry "PersonA" "WorkMobile"    "358"    "123456789"
  $ addEntry "PersonD" "WorkLandline"  "+358"   "123456789"
  $ addEntry "PersonA" "WorkLandline"  "358"    "123456789"
  $ addEntry "PersonA" "WorkMobile"    "00358"  "123456789"
  $ addEntry "PersonA" "WorkMobile"    "358"    "987654321"
  $ addEntry "PersonB" "WorkLandline"  "358"    "2323"
  $ addEntry "PersonB" "Other"         "+358"   "144"
  $ addEntry "PersonC" "WorkLandline"  "358"    "12312123" EmptyPhoneBook
