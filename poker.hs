import Data.List

data Rank =
    HighCard [Kind] |
    OnePair Kind |
    OnePairWithOneKicker Kind Kind |
    OnePairWithTwoKicker Kind Kind Kind |
    OnePairWithThreeKicker Kind Kind Kind Kind |
    TwoPair Kind Kind (Maybe Kind) |
    Triple Kind |
    TripleWithOneKicker Kind Kind |
    TripleWithTwoKicker Kind Kind Kind |
    Straight Kind |
    Flush Kind Kind Kind Kind Kind |
    FullHouse Kind Kind |
    FourOfAKind Kind (Maybe Kind) |
    StraightFlush Kind deriving (Eq, Ord, Show)

getKind :: Card -> Kind
getKind (Card kind _) = kind

getSuit :: Card -> Suit
getSuit (Card _ suit) = suit

allStraights :: [[Kind]] 
allStraights = lowestStraight:otherStraights where
    lowestStraight = [Two,Three,Four,Five,Ace]
    otherStraights = map straight [0..length kinds - size] where
        straight start = take size $ drop start kinds
        kinds = [minBound :: Kind .. maxBound]
        size = 5

getRank5 :: [Card] -> Rank
getRank5 cards
    | isStraight && isFlush = StraightFlush $ last sortedKinds
    | isFourOfAKind = if length kindGroupsSorted == 2 then FourOfAKind (head kindGroupsSorted) (Just (kindGroupsSorted !! 1)) else FourOfAKind (head kindGroupsSorted) Nothing
    | isFullHouse = FullHouse (head kindGroupsSorted) (kindGroupsSorted !! 1)
    | isFlush = Flush (sortedKinds !! 4) (sortedKinds !! 3) (sortedKinds !! 2) (sortedKinds !! 1) (head sortedKinds)
    | isStraight = Straight $ last sortedKinds
    | isTriple = case length kindGroupsSorted of
        1 -> Triple (head kindGroupsSorted)
        2 -> TripleWithOneKicker (head kindGroupsSorted) (kindGroupsSorted !! 1)
        3 -> TripleWithTwoKicker (head kindGroupsSorted) (kindGroupsSorted !! 1) (kindGroupsSorted !! 2)
    | length pairs == 2 = case length kindGroupsSorted of
        2 -> TwoPair (pairs !! 0) (pairs !! 1) Nothing
        3 -> TwoPair (pairs !! 0) (pairs !! 1) (Just (kindGroupsSorted !! 2)) where
    | length pairs == 1 = case length kindGroupsSorted of
        1 -> OnePair (pairs !! 0)
        2 -> OnePairWithOneKicker (pairs !! 0) (head onePairKickers)
        3 -> OnePairWithTwoKicker (pairs !! 0) (head onePairKickers) (onePairKickers !! 1)
        4 -> OnePairWithThreeKicker (pairs !! 0) (head onePairKickers) (onePairKickers !! 1) (onePairKickers !! 2)
    | otherwise = HighCard sortedKinds where
        isFlush = len == 5 && (length $ nub $ map getSuit sortedCards) == 1
        isStraight = any (sortedKinds ==) allStraights
        isFullHouse = len == 5 && numGroups == 2 && lenHeadGroup == 3
        isFourOfAKind = lenHeadGroup == 4
        isTriple = lenHeadGroup == 3 && not isFullHouse
        pairs = reverse $ sort $ map head $ filter ((== 2) . length) sortByGroupSize
        isOnePair = (length $ filter ((== 2) . length) sortByGroupSize) == 1
        len = length sortedCards
        lenHeadGroup = length $ head sortByGroupSize
        numGroups = length sortByGroupSize
        kindGroupsSorted :: [Kind] = map head sortByGroupSize
        sortByGroupSize :: [[Kind]] = sortBy compareLength $ group sortedKinds
        sortedCards = sort cards
        sortedKinds = map getKind sortedCards
        compareLength g1 g2 = length g2 `compare` length g1
        onePairKickers = reverse $ sort $ tail kindGroupsSorted

getRank :: [Card] -> Rank
getRank cards 
    | length cards <= 5 = getRank5 cards
    | otherwise = maximum $ map getRank5 $ nOverK 5 cards

data Card = Card Kind Suit deriving (Eq, Ord)
data Suit = Diamonds | Hearts | Spades | Clubs deriving (Eq, Enum, Bounded, Ord)
data Kind = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Enum, Bounded, Ord)

instance Show Suit where
    show Clubs    = "c"
    show Spades   = "s"
    show Hearts   = "h"
    show Diamonds = "d"

instance Show Kind where
    show Ace    = "A"
    show King   = "K"
    show Queen  = "Q"
    show Jack   = "J"
    show Ten    = "T"
    show Nine   = "9"
    show Eight  = "8"
    show Seven  = "7"
    show Six    = "6"
    show Five   = "5"
    show Four   = "4"
    show Three  = "3"
    show Two    = "2"

instance Show Card where
    show (Card k s) = show k ++ show s

type CardSet = [Card]

deck :: CardSet
deck = [Card k s | k <- [minBound .. maxBound], s <- [minBound .. maxBound]]

nOverKIndices :: Int -> Int -> [[Int]]
nOverKIndices n k 
    | k <= 0 || k > n  = []
    | k == 1 || k == n = map (\s -> [s]) [0 .. n-1]
    | k <= 2           = [[i,j] | j <- [0 .. n-1], i <- [j+1 .. n-1]]
    | otherwise        = [a:h:t | h:t <- nOverKIndices n (k-1), a <- [h+1 .. n-1 ]]

nOverK :: Eq a => Int -> [a] -> [[a]]
nOverK k list = map fromIndex $ nOverKIndices n k where
    fromIndex = map (set !!)
    n = length set
    set = nub list


