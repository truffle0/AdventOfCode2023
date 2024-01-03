module General (pairs, diff, unmaybe)
where
    pairs :: [a] -> [(a,a)]
    pairs [] = []
    pairs [_] = []
    pairs (x:y:as) = (x, y) : pairs (y:as)

    diff :: Int -> Int -> Int
    diff x y = y - x

    unmaybe :: [Maybe a] -> [a]
    -- Removes Nothings from a list, leaving just the unpacked Justs
    unmaybe [] = []
    unmaybe (a:at) = case a of
        Nothing -> unmaybe at
        Just x -> x : unmaybe at