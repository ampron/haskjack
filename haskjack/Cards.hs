
module Cards
( CardSuit
, CardValue
, Card
, isValue
, isSuit
, isFaceCard
, blackjackValue
, scoreCards
, freshDeck
) where

data CardSuit = Hearts | Diamonds | Spades | Clubs
    deriving (Eq, Ord)

instance Show CardSuit where
    show Hearts   = "♡"
    show Diamonds = "♢"
    show Spades   = "♠"
    show Clubs    = "♣"

instance Enum CardSuit where
    fromEnum Hearts   = 0
    fromEnum Diamonds = 1
    fromEnum Spades   = 2
    fromEnum Clubs    = 3
    
    toEnum 0 = Hearts
    toEnum 1 = Diamonds
    toEnum 2 = Spades
    toEnum _ = Clubs

data CardValue = Two | Three | Four | Five | Six | Seven | Eight |
                 Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Ord)

instance Show CardValue where
    show Two   = "2"
    show Three = "3"
    show Four  = "4"
    show Five  = "5"
    show Six   = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine  = "9"
    show Ten   = "10"
    show Jack  = "J"
    show Queen = "Q"
    show King  = "K"
    show Ace   = "A"

instance Enum CardValue where
    fromEnum Two   = 2
    fromEnum Three = 3
    fromEnum Four  = 4
    fromEnum Five  = 5
    fromEnum Six   = 6
    fromEnum Seven = 7
    fromEnum Eight = 8
    fromEnum Nine  = 9
    fromEnum Ten   = 10
    fromEnum Jack  = 11
    fromEnum Queen = 12
    fromEnum King  = 13
    fromEnum Ace   = 14
    
    toEnum 2  = Two
    toEnum 3  = Three
    toEnum 4  = Four
    toEnum 5  = Five
    toEnum 6  = Six
    toEnum 7  = Seven
    toEnum 8  = Eight
    toEnum 9  = Nine
    toEnum 10 = Ten
    toEnum 11 = Jack
    toEnum 12 = Queen
    toEnum 13 = King
    toEnum 14 = Ace

data Card = Card CardValue CardSuit deriving (Eq)

instance Show Card where
    show (Card val suit) = show val ++ show suit

isValue :: CardValue -> Card -> Bool
isValue testValue (Card cardVal _) = testValue == cardVal

isSuit :: CardSuit -> Card -> Bool
isSuit testSuit (Card _ cardSuit) = testSuit == cardSuit

isFaceCard :: Card -> Bool
isFaceCard (Card Jack  _) = True
isFaceCard (Card Queen _) = True
isFaceCard (Card King  _) = True
isFaceCard _              = False

freshDeck :: [Card]
freshDeck = concat [[Card n suit | n <- [Two .. Ace]] | suit <- [Hearts .. Clubs]]

blackjackValue :: Card -> Int
blackjackValue (Card Two _)   = 2
blackjackValue (Card Three _) = 3
blackjackValue (Card Four _)  = 4
blackjackValue (Card Five _)  = 5
blackjackValue (Card Six _)   = 6
blackjackValue (Card Seven _) = 7
blackjackValue (Card Eight _) = 8
blackjackValue (Card Nine _)  = 9
blackjackValue (Card Ten _)   = 10
blackjackValue (Card Jack _)  = 10
blackjackValue (Card Queen _) = 10
blackjackValue (Card King _)  = 10
blackjackValue (Card Ace _)   = 11

scoreCards :: [Card] -> Int
scoreCards cards =
    let allNonAces = filter (not . isValue Ace) cards
        nonAcesSum = sum $ map blackjackValue allNonAces
        numAces = length cards - length allNonAces
        getBest currSum 0 = currSum
        getBest currSum remaingNumAces
            | 21 < goBig = goSmall
            | otherwise  = goBig
            where goBig = getBest (currSum + 11) (remaingNumAces - 1)
                  goSmall = getBest (currSum + 1) (remaingNumAces - 1)
    in getBest nonAcesSum numAces
