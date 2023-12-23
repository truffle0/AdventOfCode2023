module Main (main)
where
    import System.IO
    import Control.Exception (IOException, catch)
    import System.Environment (getArgs)
    import Navigate (Node, name, right, left, Atlas, destination, ntree, createAtlas, followDir)
    
    readlines :: Handle -> IO [String]
    readlines src = do
        line <- hGetLine src `catch` (\(e::IOException) -> return [])
        eof <- hIsEOF src
        if eof
            then return []
            else do
                next <- readlines src
                return (line : next)

    readFromFile :: FilePath -> IO [String]
    readFromFile path = do
        file <- openFile path ReadMode
        readlines file

    processInput :: [String] -> (String, Atlas)
    processInput input = (head input, createAtlas (tail input) "AAA" "ZZZ")

    partOne :: String -> Atlas -> Int
    partOne steps atlas = findEnd 0 nodeStream (destination atlas)
        where
            nodeStream = followDir (cycle steps) (ntree atlas)
            findEnd :: Int -> [Node] -> String -> Int
            findEnd i (n:ns) end
                | name n == end = i
                | otherwise = findEnd (i+1) ns end
    
    main :: IO ()
    main = do
        args <- getArgs
        input <- if not (null args)
            then readFromFile (head args)
            else readlines stdin
        
        let (steps, navi) = processInput input
        putStrLn $ "Part 1: " ++ show (partOne steps navi)