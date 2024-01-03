{-# LANGUAGE BangPatterns #-}
module Extrapolate (ExtraPol, parseExtrapol, base, row, rows, predict, extend, backtrack)
where
    import General (pairs, diff, unmaybe)
    import Text.Read (readMaybe)
    
    --- Extrapolation Tree data ---
    data ExtraPol = Head Int Int ExtraPol
        | Diff [Int] ExtraPol
        | Base [Int]

    instance Show ExtraPol where
        show (Head dep wid rows) = show wid++ " x " ++ show dep ++ ":\n" ++ show rows
        show (Diff row next) = show row ++ "\n" ++ show next
        show (Base xs) = show xs
    
    width :: ExtraPol -> Int
    width (Head width _ _) = width
    width (Diff _ next) = width next
    width (Base row) = length row

    depth :: ExtraPol -> Int
    depth (Head _ depth _) = depth
    depth (Diff _ next) = 1 + depth next
    depth (Base _) = 1

    base :: ExtraPol -> [Int]
    base (Head _ _ start) = base start
    base (Diff _ next) = base next
    base (Base row) = row

    row :: ExtraPol -> [Int]
    row (Diff row _) = row
    row (Base row) = row

    rows :: ExtraPol -> [[Int]]
    rows (Head _ _ start) = rows start
    rows (Diff this next) = this : rows next
    rows (Base this) = [this]

    --- Functional Functions ---

    rowDiffs :: [Int] -> [Int]
    rowDiffs [] = error "empty list!"
    rowDiffs [x] = [0]
    rowDiffs xs = map (uncurry diff) (pairs xs)

    recParseRows :: ExtraPol -> ExtraPol
    recParseRows last
        | not $ all (== 0) next = recParseRows $ Diff next last
        | otherwise = Diff next last
        where
            !next = rowDiffs (row last)

    parseExtrapol :: String -> ExtraPol
    parseExtrapol [] = error "empty line!"
    parseExtrapol line = Head dep wid rows
        where
            nums = unmaybe $ map readMaybe $ words line

            !rows = recParseRows (Base nums)
            wid = width rows
            dep = depth rows
    
    predict :: ExtraPol -> Int
    predict pol = recPredict 0 (rows pol)
        where
            recPredict :: Int -> [[Int]] -> Int
            recPredict i [] = i
            recPredict i (x:xs) = recPredict (last x + i) xs
    
    extend :: ExtraPol -> ExtraPol
    extend (Head wid dep rows) = Head (wid+1) dep (extend rows)
    extend (Base base) = Base base
    extend (Diff base downstream)
        | all (== 0) base = Diff (base ++ [0]) $ extend nextNode
        | otherwise = Diff base $ extend nextNode
        where
            nextNode = case downstream of
                Diff row next -> Diff (row ++ [last row + last base]) next
                Base row -> Base (row ++ [last row + last base])
    
    backtrack :: ExtraPol -> ExtraPol
    backtrack (Head wid dep rows) = Head (wid+1) dep (backtrack rows)
    backtrack (Base base) = Base base
    backtrack (Diff base downstream)
        | all (== 0) base = Diff ( 0 : base) $ backtrack nextNode
        | otherwise = Diff base $ backtrack nextNode
        where
            nextNode = case downstream of
                Diff row next -> Diff (head row - head base : row) next
                Base row -> Base (head row - head base : row)