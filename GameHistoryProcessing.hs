{-
    This game history processing is referring to Assignment 2 and practical class's code
-}

module GameHistoryProcessing where

    import qualified Data.List.Split as K

    data HistoryRecord = HistoryRecord {name :: String, roundSur :: String} deriving Show

    file :: IO String 
    file = readFile "GamePlayHistory.txt"

    dataRows :: IO [String]
    dataRows = file >>= return . lines

    -- find the rows with actual raw data (by removing first row)
    rawDataRowsList :: IO [[String]]  
    rawDataRowsList = dataRows >>= return . tail . map (\row -> (K.splitOn "," row))

    -- to extract the first row which is the header row
    headerRow :: IO [String]
    headerRow = dataRows >>= return . K.splitOn "," . head

    -- parse history record into datatype of HistoryRecord
    parseHistoryRecord :: [[String]] -> [HistoryRecord]
    parseHistoryRecord records = map (\rowList -> (HistoryRecord (rowList !! 0) (rowList !! 1))) records

    -- parse data into HistoryRecord data constructor 
    players :: IO [HistoryRecord]
    players = rawDataRowsList >>= return . parseHistoryRecord

    -- get the text for displaying
    displayRecord :: HistoryRecord -> IO String
    displayRecord x = players >>= return . (\a -> (name x) <> " had survived until Round " <> (roundSur x))

    -- to display all the game history records
    displayAllRecords :: IO[()]
    displayAllRecords = players >>= (mapM displayRecord) >>= mapM putStrLn

    -- to append the player name and number of round survice into the txt file
    writeToFile :: String -> String -> IO ()
    writeToFile x y = appendFile "GamePlayHistory.txt" ("\n" ++ x ++ "," ++ y)

    -- to record the game history
    recordGameHistory :: String -> IO ()
    recordGameHistory round = do
        putStr "Please enter your player name: "
        name <- getLine
        putStrLn ("You can view your game play history at the Game History Page.")
        if name == []
            then writeToFile "Unknown" round
            else writeToFile name round