module Main (main)
where
    import System.IO
    import Control.Exception (IOException, catch)
    import System.Environment (getArgs)

    import Extrapolate (ExtraPol, parseExtrapol, base, extend, backtrack)

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

    processInput :: [String] -> [ExtraPol]
    processInput = map parseExtrapol

    partOne :: [ExtraPol] -> Int
    partOne extras = sum $ map (last . base . extend) extras

    partTwo :: [ExtraPol] -> Int
    partTwo extras = sum $ map (head . base . backtrack) extras

    main :: IO ()
    main = do
        args <- getArgs
        input <- if not (null args)
            then readFromFile (head args)
            else readlines stdin
        
        let extrapols = processInput input

        putStrLn $ "Part 1: " ++ show (partOne extrapols)
        putStrLn $ "Part 2: " ++ show (partTwo extrapols)