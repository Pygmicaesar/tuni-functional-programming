-- define PhoneType
data PhoneType = WorkLandLine | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read)

-- type synonyms for Integer
type CountryCode = Integer
type PhoneNo = Integer

-- define Phone with record syntax
data Phone = Phone { phoneType :: PhoneType
                   , countryCode :: CountryCode
                   , phoneNo :: PhoneNo
                   } deriving (Show, Eq, Read)

-- function that creates a new Phone
phone :: PhoneType -> CountryCode -> PhoneNo -> Phone
phone pt cc pn
  | cc < 0 || pn < 0  = error "The country code and phone number can't be negative!"
  | otherwise         = Phone {phoneType = pt, countryCode = cc, phoneNo = pn}