module Main (main)
where
    import System.IO
    import Control.Exception (IOException, catch)
    import System.Environment (getArgs)
    import Navigate (Path, name, left, right, asRef, WorldMap, paths, newWorldMap, place, followDir, SimpleDir, charToDir)
    import Data.List (findIndex, findIndices)
    import General (bisect, ordIntersect, condense, limit)
    import Data.Maybe (fromJust)

    readlines :: Handle -> IO [String]
    readlines src = do
        eof <- hIsEOF src
        if eof
            then return []
            else do
                line <- hGetLine src `catch` (\(e::IOException) -> return [])
                next <- readlines src
                return (line : next)

    readFromFile :: FilePath -> IO [String]
    readFromFile path = do
        file <- openFile path ReadMode
        readlines $! file

    processInput :: [String] -> ([SimpleDir], WorldMap String)
    processInput input = (map charToDir (head input), newWorldMap (tail input))

    partOne :: [SimpleDir] -> WorldMap String -> Int
    partOne steps wMap = fromJust $ findIndex (\n -> name n == "ZZZ") route
        where
            loc = place (asRef "AAA") wMap
            route = followDir (cycle steps) loc

    partTwo :: [SimpleDir] -> WorldMap String -> Int
    partTwo dir wMap = fromJust $ findIndex (all (\x -> last (name x) == 'Z')) (bisect routes `limit` 1000000000)
        where
            nodes = paths $! wMap
            starts = map (`place` wMap) $ filter (\n -> last (name n) == 'A') nodes
            routes = map (followDir (cycle dir)) $! starts

    main :: IO ()
    main = do
        args <- getArgs
        input <- if not (null args)
            then readFromFile (head args)
            else readlines stdin

        let (steps, wMap) = processInput $! input
        putStrLn $ "Part 1: " ++ show (partOne steps wMap)
        putStrLn $ "Part 2: " ++ show (partTwo steps wMap)