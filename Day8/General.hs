module General (unmaybe, unempty, both, ends, extract, indexWhere, bisect, ordIntersect, condense, limit)
where
    -- Most functions here are unused, but were important when experimenting

    unmaybe :: [Maybe a] -> [a]
    -- Removes Nothings from a list, leaving just the unpacked Justs
    unmaybe [] = []
    unmaybe (a:at) = case a of
        Nothing -> unmaybe at
        Just x -> x : unmaybe at

    unempty :: [[a]] -> [[a]]
    -- Intended to filter out empty String from a list
    unempty [] = []
    unempty (a:at)
        | null a = unempty at
        | otherwise = a : unempty at

    both :: Maybe a -> Maybe b -> Maybe (a, b)
    -- Only returns when both have a value
    both (Just a) (Just b) = Just (a, b)
    both _ _ = Nothing

    ends :: [a] -> (a, a)
    -- Returns both the head and last elements of a list
    ends [] = error "Empty list!"
    ends as = (head as, last as)

    extract :: (a->Bool) -> [a] -> (Maybe a, [a])
    -- Extract the first element that satisfies the input func
    -- then return both this element and the list without the element
    extract func [] = (Nothing, [])
    extract func (x:xs)
        | func x = (Just x, xs)
        | otherwise = (e, x : es)
            where
                (e, es) = extract func xs

    indexWhere :: (a -> Bool) -> [a] -> [Int]
    indexWhere func as = indexes func as 0
        where
        indexes :: (a -> Bool) -> [a] -> Int -> [Int]
        indexes _ [] _ = error "reached end with no match"
        indexes func (a:as) i
            | func a = i : indexes func as (i+1)
            | otherwise = indexes func as (i+1)
    
    bisect :: [[a]] -> [[a]]
    -- Slices to top off every list in the list
    -- returns a list of each layer
    bisect [] = []
    bisect as = map head as : bisect (map tail as)

    ordIntersect :: Ord a => [a] -> [a] -> [a]
    ordIntersect [] _ = []
    ordIntersect _ [] = []
    ordIntersect (a:as) (b:bs) =
        case a `compare` b of
            LT -> ordIntersect as (b:bs)
            EQ -> a : ordIntersect as bs
            GT -> ordIntersect (a:as) bs

    condense :: (a -> a -> a) -> [a] -> a
    condense _ [] = error "impossible to condense an empty list"
    condense _ [x] = x
    condense f as = condense f $ pass f as
        where
            pass :: (a -> a -> a) -> [a] -> [a]
            pass _ [] = []
            pass _ [x] = [x]
            pass f (x:y:as) = f x y : pass f as
    
    limit :: [a] -> Int -> [a]
    limit [] _ = []
    limit (x:xs) i
        | i <= 0 = error "iteration limit reached!"
        | otherwise = x : xs `limit` (i - 1)