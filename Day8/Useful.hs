module Useful (unmaybe, unempty, both, ends)
where
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