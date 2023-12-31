module Navigate (Path, name, left, right, asRef, WorldMap, paths, makeMap, newWorldMap, place, followDir, SimpleDir, charToDir)
where
    import Text.Regex.TDFA
    import General (unmaybe)

    import Data.Map (Map, (!), elems, member)
    import qualified Data.Map as Map
    
    --- Path data type ---
    data Path a = (Eq a, Ord a) => Node { name::a, left::Path a, right::Path a } | Ref { name::a }

    instance Show a => Show (Path a) where
        show (Node n left right) = show n ++ " = (" ++ show (name left) ++ ", " ++ show (name right) ++ ")"
        show (Ref n) = show n

    asRef :: a -> Path a
    asRef = Ref

    --- World Map type ---
    newtype WorldMap a = WorldMap { refer::Map a (Path a) }

    paths :: WorldMap a -> [Path a]
    paths (WorldMap wMap) = elems wMap

    --- Simple Direction type ---
    data SimpleDir = DLeft | DRight

    charToDir :: Char -> SimpleDir
    charToDir 'L' = DLeft
    charToDir 'R' = DRight
    charToDir _ = error "That's not a direction!"

    --- Functional functions ---
    readNode :: String -> Maybe (Path String)
    readNode st
        | st =~ re :: Bool = Just (Node name (Ref left) (Ref right))
        | otherwise = Nothing
        where
            re = "([A-Z]+) = \\(([A-Z]+), ([A-Z]+)\\)"
            (_:name:left:right:_) = getAllTextSubmatches (st =~ re) :: [String]
    
    parseNodes :: [String] -> [Path String]
    parseNodes strs = unmaybe $ map readNode strs

    makeMap :: Ord a => [Path a] -> Map a (Path a)
    makeMap nodes = Map.fromList $! map (\n -> (name n, n)) nodes

    continuity :: Ord a => WorldMap a -> Bool
    continuity (WorldMap wmap) = all childPresent nodes
        where
            nodes = elems wmap
            childPresent n = (name (left n) `member` wmap) && (name (right n) `member` wmap)

    place :: Ord a => Path a -> WorldMap a -> Path a
    place (Ref nod) wmap = place (refer wmap ! nod) wmap
    place (Node name left right) nodes = Node name (place left nodes) (place right nodes)

    newWorldMap :: [String] -> WorldMap String
    newWorldMap st
        | continuity wmap = wmap
        | otherwise = error "Map incomplete!"
        where
            wmap = WorldMap $! makeMap $ parseNodes st
    
    followDir :: [SimpleDir] -> Path a -> [Path a]
    followDir [] _ = []
    followDir (DRight:ds) nod = Ref (name nod) : followDir ds (right nod)
    followDir (DLeft:ds) nod = Ref (name nod) : followDir ds (left nod)