module Main (Main.main)
where
    import System.IO
    import System.Exit
    import System.Environment
    import Control.Monad
    import Camelcards (Hand, Card, readHand)
    import Data.List.Split (splitOn)
    import Text.Read (readMaybe)
    import Data.List (sortBy)
    import Data.Function (on)
    import Control.Exception (IOException, catch)
    import Data.Maybe (fromMaybe, fromJust)
    
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


    both :: Maybe a -> Maybe b -> Maybe (a, b)
    both (Just a) (Just b) = Just (a, b)
    both _ _ = Nothing

    processLine :: String -> Maybe (Hand, Int)
    processLine str = both (readHand hand) (readMaybe bet)
        where
            split = splitOn " " str
            hand = head split
            bet = last split
    
    processInput :: [String] -> [(Hand, Int)]
    processInput [] = []
    processInput (s:st) =
        case processLine s of
            Nothing -> processInput st
            (Just x) -> x : processInput st

    partOne :: [(Hand, Int)]->Int
    partOne plays = sum values
        where
            values = score 1 $ sortBy (compare `on` fst) plays

            score :: Int -> [(Hand, Int)] -> [Int]
            score _ [] = []
            score i ((_, bet):st) = (bet * i) : score (i + 1) st

    main :: IO ()
    main = do
        args <- getArgs
        input <- if not (null args)
            then readFromFile (head args)
            else readlines stdin

        let plays = processInput input
        putStrLn $ "Part 1: " ++ show (partOne plays)