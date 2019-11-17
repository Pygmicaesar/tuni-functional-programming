-- Define a classic deck of cards
cards = [(x,y) | x <- ['s','h','c','d'], y <- [2..14]]

-- Define the Card type
type Card = (Char,Int)

-- Function for calculating the player's score
cardGame :: Card -> Card -> Int
cardGame card1 card2 
  | not (elem card1 cards) || not (elem card2 cards) = error "These are not valid cards!"
  | card1 == card2 = error "A deck of cards can't have duplicates!"
  | checkAceOfSpades card1 || checkAceOfSpades card2 = 14
  | checkStraight card1 card2 && (fst card1 == fst card2) = 8
  | snd card1 == snd card2 = 6
  | checkStraight card1 card2 = 4
  | fst card1 == fst card2 = 2 
  | otherwise = 0

-- Function that checks whether a card is the ace of spades
checkAceOfSpades :: Card -> Bool
checkAceOfSpades ('s',14) = True
checkAceOfSpades _ = False

-- Function that checks whether two cards form a straight, now fixed to use less function calls overall
checkStraight :: Card -> Card -> Bool
checkStraight card1 card2 = abs (snd card1 - snd card2) == 1