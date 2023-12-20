module Camelcards(Hand, Card, readHand)
where
    import Data.List (group, sortBy, sort)
    import Data.Function (on)
    import Data.Maybe (fromJust)
    
    --- Implementation of Card type ---
    data Card = Ace | King | Queen | Jack | Num Int

    rank :: Card -> Int
    -- Integer ranking of each Card
    rank Ace = 14
    rank King = 13
    rank Queen = 12
    rank Jack = 11
    rank (Num i) = i

    instance Eq Card where
        (==) a b = rank a == rank b

    instance Ord Card where
        compare a b = compare (rank a) (rank b)

    instance Show Card where
        show Ace = "A"
        show King = "K"
        show Queen = "Q"
        show Jack = "J"
        show (Num i) = show i


    --- Implementation of Hand types ---
    data HandValue = FiveOfAKind | FullHouse | FourOfAKind
        | ThreeOfAKind | TwoPair | OnePair | HighCard
        deriving (Eq, Ord, Bounded, Enum)

    instance Show HandValue where
        show FiveOfAKind = "Five of a Kind"
        show FullHouse = "Full House"
        show FourOfAKind = "Four of a Kind"
        show ThreeOfAKind = "Three of a kind"
        show TwoPair = "Two Pair"
        show OnePair = "One Pair"
        show HighCard = "High Card"


    --- Implementation of a encapsulating hand variable ---
    data Hand = Hand {value::HandValue, cards::[Card]}

    instance Eq Hand where
        (==) (Hand a ac) (Hand b bc) = a == b && and (zipWith (==) ac bc)

    instance Ord Hand where
        compare a b =
            if rank a /= rank b then compare (rank a) (rank b)
            else compareByCards (cards a) (cards b)
            where
                rank :: Hand->Int
                rank (Hand value _) = case value of
                    FiveOfAKind -> 7
                    FourOfAKind -> 6
                    FullHouse -> 5
                    ThreeOfAKind -> 4
                    TwoPair -> 3
                    OnePair -> 2
                    HighCard -> 1

                compareByCards :: [Card]->[Card]->Ordering
                compareByCards (a:as) (b:bs) =
                    if a == b then compareByCards as bs
                    else compare a b

    instance Show Hand where
        show (Hand value cards) = show value ++ ": " ++ map (head . show)  cards


    readCard :: Char -> Maybe Card
    readCard c =
        case c of
        'A' -> Just Ace
        'K' -> Just King
        'Q' -> Just Queen
        'J' -> Just Jack
        '2' -> Just $ Num 2
        '3' -> Just $ Num 3
        '4' -> Just $ Num 4
        '5' -> Just $ Num 5
        '6' -> Just $ Num 6
        '7' -> Just $ Num 7
        '8' -> Just $ Num 8
        '9' -> Just $ Num 9
        'T' -> Just $ Num 10
        _ -> Nothing

    arrangeHand :: [Card] -> Maybe Hand
    arrangeHand xs
        | length xs /= 5 = Nothing
        | all (== head xs) (tail xs) = Just (Hand FiveOfAKind xs)
        | length (head gs) == 3 && length (last gs) == 2 = Just (Hand FullHouse xs)
        | length (head gs) == 4 = Just (Hand FourOfAKind xs)
        | length gs == 3 && length (head gs) == 3 = Just (Hand ThreeOfAKind xs)
        | length (head gs) == 2 && length (gs !! 1) == 2 = Just (Hand TwoPair xs)
        | length gs == 4 && length (head gs) == 2 = Just (Hand OnePair xs)
        | length gs == 5 = Just (Hand HighCard xs)
        | otherwise = Nothing
        where
            -- Group by equality, and sort groups descending by length
            gs = sortBy (flip compare `on` length) (group $ sort xs)
    
    readHand :: String -> Maybe Hand
    readHand st
        | Nothing `elem` cards = Nothing
        | otherwise = arrangeHand $ map fromJust cards
        where
            cards = map readCard st