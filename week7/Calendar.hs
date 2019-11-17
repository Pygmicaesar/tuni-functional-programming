import Data.List
import Data.Function
import Data.Ord

-- Month implementation
data Month = Month Integer deriving (Eq, Read, Ord)

toMonth :: Integer -> Month
toMonth x
  | x < 1     = error "Minimum month number is 1" 
  | x > 12     = error "Maximum month number is 12" 
  | otherwise = Month x

fromMonth :: Month -> Integer
fromMonth (Month i) = i  -- Pattern match i out

instance Show Month where
  show (Month month)
    | month < 10 = '0' : show month
    | otherwise = show month


-- Day implementation
data Day = Day Integer deriving (Eq, Read, Ord)

toDay :: Integer -> Day
toDay x
  | x < 1     = error "Minimum day number is 1" 
  | x > 31     = error "Maximum day number is 31" 
  | otherwise = Day x

fromDay :: Day -> Integer
fromDay (Day i) = i

instance Show Day where
  show (Day day)
    | day < 10 = '0' : show day
    | otherwise = show day


-- Year implementation
newtype Year = Year Integer deriving (Eq, Read, Ord)

toYear :: Integer -> Year
toYear x
  | x == 0 = error "No year 0"
  | otherwise = Year x

fromYear :: Year -> Integer
fromYear (Year x) = x

instance Show Year where
  show (Year year) = show year


-- Date implementation
data Date = Date { year :: Year, month :: Month, day :: Day } deriving (Eq, Ord)

instance Show Date where
  show (Date year month day) = show year ++ "-" ++ show month ++ "-" ++ show day


-- Date functions
-- A function to check if a year is a leap year
leapYear (Year y)
  | mod y 400 == 0 = True
  | mod y 100 == 0 = False
  | mod y 4 == 0 = True
  | otherwise = False

makeMaybeDate :: Integer -> Integer -> Integer -> Maybe Date
makeMaybeDate y m d
  | y == 0 = Nothing
  | elem m [1,3,5,7,8,10,12] &&
    elem d [1..31] = makeJustDate y m d
  | elem m [4,6,9,11] &&
    (elem d [1..30]) = makeJustDate y m d
  | m==2 && elem d [1..28] = makeJustDate y m d
  | leapYear (toYear y) && m==2 && d==29 = makeJustDate y m d
  | otherwise = Nothing
  where makeJustDate y m d = Just Date {year = toYear y, month = toMonth m, day = toDay d}


readDate :: String -> Maybe Date
readDate x =
  case groupBy ((==) `on` (/= '-')) x of
    (year:"-":month:"-":day:[]) -> 
      let year'   = read year :: Integer
          month'  = read month :: Integer
          day'    = read day :: Integer
      in makeMaybeDate year' month' day'
    _ -> Nothing


doCommand :: String -> IO [EventInfo] -> IO ()
doCommand input ioEvents = do
  events <- ioEvents

  case groupBy ((==) `on` (/= '\'')) input of

    ("Event ":"'":name:"'":" happens at ":"'":place:"'":" on ":"'":date:"'":[]) -> do
      let cleanDate = readDate date
      case cleanDate of
        Just x -> do
          let newEvents = addEvent (EventInfo name place (unMaybe cleanDate)) events
          putStrLn "ok"
          loop $ return newEvents
        Nothing -> do
          putStrLn "Bad date"
          loop $ return events

    ("Tell me about ":"'":name:"'":[]) -> do
      let event = findEventByName name events
      case event of
        Just (EventInfo n p d) -> do
          putStrLn $ "Event " ++ n ++ " happens at " ++ p ++ " on " ++ show d
        Nothing -> do
          putStrLn "I do not know of such event"
      loop $ return events

    ("What happens on ":"'":date:"'":[]) -> do
      let cleanDate = readDate date
          printByDate = True
      case cleanDate of
        Just x -> do
          let eventsByDate = sortByName $ findEventsByDate (unMaybe cleanDate) events
          if eventsByDate /= [] then do
            printList eventsByDate printByDate
          else do
            putStrLn "Nothing that I know of"
        Nothing -> do
          putStrLn "Bad date"
      loop $ return events

    ("What happens at ":"'":place:"'":[]) -> do
      let eventsByPlace = sortByName $ findEventsByPlace place events
          printByDate = False
      if eventsByPlace /= [] then do
        printList eventsByPlace printByDate
      else do
        putStrLn "Nothing that I know of"
      loop $ return events

    _ -> do
      putStrLn $ 
        "I do not understand that. I understand the following:\n" ++
        "*Event <name> happens at <place> on <date>\n" ++
        "*Tell me about <eventname>\n" ++
        "*What happens on <date>\n" ++
        "*What happens at <place>\n" ++
        "*Quit"
      loop $ return events


addEvent :: EventInfo -> [EventInfo] -> [EventInfo]
addEvent x [] = [x]
addEvent a@(EventInfo name place date) (b@(EventInfo name' _ _) : xs)
  | name == name' = a : xs
  | otherwise = b : addEvent a xs

findEventByName :: String -> [EventInfo] -> Maybe EventInfo
findEventByName eventName [] = Nothing
findEventByName eventName (a@(EventInfo name _ _) : xs)
  | eventName == name = Just a
  | otherwise = findEventByName eventName xs

findEventsByDate :: Date -> [EventInfo] -> [EventInfo]
findEventsByDate eventDate [] = []
findEventsByDate eventDate (a@(EventInfo _ _ date) : xs)
  | eventDate == date = a : findEventsByDate eventDate xs
  | otherwise = findEventsByDate eventDate xs

findEventsByPlace :: String -> [EventInfo] -> [EventInfo]
findEventsByPlace eventPlace [] = []
findEventsByPlace eventPlace (a@(EventInfo _ place _) : xs)
  | eventPlace == place = a : findEventsByPlace eventPlace xs
  | otherwise = findEventsByPlace eventPlace xs

printList :: [EventInfo] -> Bool -> IO ()
printList [] _ = return ()
printList (EventInfo name place date : xs) bool = do
  case bool of
    True -> do
      putStrLn $ "Event " ++ name ++ " happens on " ++ show date
    False -> do
      putStrLn $ "Event " ++ name ++ " happens at " ++ place
  printList xs bool

unMaybe :: Maybe a -> a
unMaybe (Just x) = x
unMaybe _ = error "Can't make a defined type out of that!"

sortByName :: [EventInfo] -> [EventInfo]
sortByName = sortBy (comparing name)