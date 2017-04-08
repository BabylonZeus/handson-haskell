module PokerHand where
import Data.Char
import Data.List
import Data.Ord

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Ord, Enum, Show)
data Suit = Hearts | Clubs | Diamonds | Spades
    deriving (Eq, Show)
type Card = (Rank, Suit)

charToRank 'A' = Ace
charToRank 'K' = King
charToRank 'Q' = Queen
charToRank 'J' = Jack
charToRank 'T' = Ten
charToRank c = toEnum $ (digitToInt c) - 2

charToSuit 'd' = Diamonds
charToSuit 's' = Spades
charToSuit 'h' = Hearts
charToSuit 'c' = Clubs

card :: String -> Card
card [r,s] = (charToRank r, charToSuit s)

rank :: Card -> Rank
rank = fst

suit :: Card -> Suit
suit = snd

cards :: String -> [Card]
cards = map card . words

ranks :: [Card] -> [Rank]
ranks = sortBy (flip compare) . map rank

