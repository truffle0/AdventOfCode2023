module Navigate (Node, name, right, left, Atlas, destination, ntree, createAtlas, followDir)
where
    import Text.Regex.TDFA
    import Useful (unmaybe)

    import Data.Map (Map, (!))
    import qualified Data.Map as Map
    
    --- Node data type ---
    data Node = Node { name::String, left::Node, right::Node } | Ref { name::String }

    instance Show Node where
        show (Node n left right) = n ++ " = (" ++ name left ++ ", " ++ name right ++ ")"
        show (Ref n) = n

    --- Atlas type ---
    data Atlas = Atlas { ntree::Node, start::String, destination::String }


    --- Functional functions ---
    readNode :: String -> Maybe Node
    readNode st
        | st =~ re :: Bool = Just (Node name (Ref left) (Ref right))
        | otherwise = Nothing
        where
            re = "([A-Z]+) = \\(([A-Z]+), ([A-Z]+)\\)"
            (_:name:left:right:_) = getAllTextSubmatches (st =~ re) :: [String]
    
    parseNodes :: [String] -> [Node]
    parseNodes strs = unmaybe $ map readNode strs

    nodeMap :: [Node] -> Map String Node
    nodeMap nodes = Map.fromList (map (\n -> (name n, n)) nodes)

    tree :: Node -> Map String Node -> Node
    tree (Ref nod) nodes = tree (nodes ! nod) nodes
    tree (Node name left right) nodes = Node name (tree left nodes) (tree right nodes)

    createAtlas :: [String] -> String -> String -> Atlas
    createAtlas input start = Atlas ntr start
        where
            nodes = parseNodes input
            ntr = tree (Ref start) (nodeMap nodes)
    
    followDir :: [Char] -> Node -> [Node]
    followDir [] _ = []
    followDir _ (Ref name) = error "Expected node, found reference!"
    followDir (d:ds) nod
        | d == 'R' = Ref (name nod) : followDir ds (right nod)
        | d == 'L' = Ref (name nod) : followDir ds (left nod)
        | otherwise = error "Invalid direction!"