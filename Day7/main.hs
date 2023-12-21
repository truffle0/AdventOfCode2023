module Main (Main.main)
where
    import System.IO
    import System.Environment
    import Control.Monad
    import Camelcards (Hand, Card, readHand, bestJoker, convertJacks)
    import Data.List.Split (splitOn)
    import Text.Read (readMaybe)
    import Data.List (sortBy)
    import Data.Function (on)
    import Control.Exception (IOException, catch)
    import Data.Maybe (fromMaybe, fromJust)
    import Useful (both, unmaybe, ends, unempty)
    
    readFromFile :: FilePath->IO [String]
    readFromFile path = do
        file <- openFile path ReadMode
        readlines file

    readlines :: Handle->IO [String]
    readlines src = do
        line <- hGetLine src `catch` (\(e :: IOException) -> return [])
        if null line
            then return []
            else do
                next <- readlines src
                return (line : next) 
    
    processInput :: [String] -> [(Hand, Int)]
    processInput [] = []
    processInput lines = unmaybe $ map forLine pairs
        where
            pairs = [ ends $ splitOn " " ln | ln <- lines ]

            forLine :: (String,String) -> Maybe (Hand, Int)
            forLine (hand,bet) = both (readHand hand) (readMaybe bet)
            

    score :: Int -> [(Hand, Int)] -> [Int]
    score _ [] = []
    score i ((_, bet):st) = (bet * i) : score (i + 1) st

    partOne :: [(Hand, Int)]->Int
    partOne plays = sum values
        where
            values = score 1 $ sortBy (compare `on` fst) plays
    
    partTwo :: [(Hand, Int)] -> Int
    partTwo plays = sum values
        where
            values = score 1 $ sortBy (compare `on` fst) (map toJokers plays)

            toJokers :: (Hand, Int) -> (Hand, Int)
            toJokers (hand, score) = (bestJoker (convertJacks hand), score)

    main :: IO ()
    main = do
        args <- getArgs
        input <- if not (null args)
            then readFromFile (head args)
            else readlines stdin

        let plays = processInput input
        putStrLn $ "Part 1: " ++ show (partOne plays)
        putStrLn $ "Part 2: " ++ show (partTwo plays)