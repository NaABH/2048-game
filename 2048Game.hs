import GameInterface
import GameHistoryProcessing
import Data.List
import Data.Maybe
import System.Random

-- function to process the user input at meanu page
process :: String -> IO()
process "1" = game initialBoard initialRoundSurvived >> main
process "2" = instruction >>= backToMenu
process "3" = history >>= backToMenu
process "4" = putStrLn "\nThank you for playing!" >> putStrLn "Program end...." 
process _ = putStrLn "\nERROR: Invalid input. Please try another input..." >> main

-- function to bring user back to menu when they enter any value
backToMenu :: String -> IO()
backToMenu _ = main

-- main function to be called to start the program
main :: IO()
main = menu >>= process

-- type of board with is used to store nested list of integer
type Board = [[Int]]

-- initialBoard that contain the default data
initialBoard :: Board
initialBoard = replaceElement (replicate 4 (replicate 4 0)) 2 (0,0)

-- function to display the board with format that is easier to read
displayBoard :: Board -> IO()
displayBoard xss = putStrLn $ unlines $ map (concat . intersperse "\t| " . map show) xss

-- function to move all element inside a list to left (recursive)
moveAllLeft :: [Int] -> [Int]
moveAllLeft [] = []
moveAllLeft (0:xs) = padding $ moveAllLeft xs
moveAllLeft (x:xs) = x:(moveAllLeft xs)

-- function to merge the element inside a list to left if they are the same value (recursive)
mergeToLeft :: [Int] -> [Int]
mergeToLeft [] = []
mergeToLeft (x:[]) = [x]
mergeToLeft (0:xs) = padding (mergeToLeft xs)
mergeToLeft (x:y:xs) = case (x == y) of
                        True -> padding ((x+y):mergeToLeft xs)
                        False -> x:mergeToLeft (y:xs)

-- function to add padding [0] to the end of a list (called when merge happen to maintaint the size of list)
padding :: [Int] -> [Int]
padding xs = xs ++ [0]

-- function to implement both move and merge operation (first move then merge)
moveNMerge :: [Int] -> [Int]
moveNMerge xs = mergeToLeft $ moveAllLeft xs

-- data for different type of operation
data Operation = MergeUp | MergeDown | MergeLeft | MergeRight

-- function to process the board based on the type of operation
-- idea retrieved from from https://github.com/SnowWalkerJ/Haskell-practice/blob/master/2048/Main.hs (line 48-52)
processBoard :: Operation -> Board -> Board
processBoard MergeLeft = map moveNMerge
processBoard MergeRight = map reverse . processBoard MergeLeft . map reverse
processBoard MergeUp = transpose . processBoard MergeLeft . transpose
processBoard MergeDown = transpose . processBoard MergeRight . transpose

-- function of match user input with the operation
processUserInput :: String -> Maybe Operation
processUserInput "w" = return MergeUp
processUserInput "a" = return MergeLeft
processUserInput "s" = return MergeDown
processUserInput "d" = return MergeRight
processUserInput _ = Nothing

-- function to check if the game is over (when 2048 present in the list)
gameOver :: Board -> Bool
gameOver xss = elem 2048 (concat xss)

-- function to check if the game should be continue (when NOT all operation return the same output as the original)
gameContinue :: Board -> Bool
gameContinue xss = (processBoard MergeUp xss /= xss) ||
                    (processBoard MergeDown xss /= xss) ||
					(processBoard MergeLeft xss /= xss) ||
					(processBoard MergeRight xss /= xss) 

-- function to random replace 0 in the list with 2 or 4
addNewValue :: Board -> IO Board
addNewValue xss = do
	nth_list <- randomRIO (0,3)
	nth_element <- randomRIO (0,3)
	newValue <- randomRIO (0,1)
	let preSetValue = [2, 4] -- make sure the random generated new value is 2 or 4
	if xss !! nth_list !! nth_element == 0
			then return $ replaceElement xss (preSetValue !! newValue) (nth_list, nth_element)
			else addNewValue xss

-- function to replace the element inside a list (provided coordinate (x,y))
replaceElement :: Board -> Int -> (Int, Int) -> Board
replaceElement xs a (x,y) = let lst = concat xs --lst = list
                                ord = x * 4 + y
                            in makeNestedList (take (ord) lst ++ [a] ++ drop (ord + 1) lst)

-- function to convert a list into nested list
makeNestedList :: [a] -> [[a]]
makeNestedList [] = []
makeNestedList xs = [take 4 xs] ++ makeNestedList (drop 4 xs)

-- type of roundSurvive
type RoundSurvived = Int

-- default value of roundSurvive (1)
initialRoundSurvived :: RoundSurvived
initialRoundSurvived = 1

-- function to loop the game (recursive)
game :: Board -> RoundSurvived -> IO ()
game oriboard round = do
	putStrLn ("\n" ++ "Round " ++ (show(round))) >> displayBoard oriboard >> putStr "Operation: "
	input <- getLine
	case processUserInput input of
		Nothing -> putStrLn "\nError: Invalid Input\nPlease select only from w,a,s,d..." >> game oriboard round
		Just operation -> do
			newBoard <- return $ processBoard operation oriboard
			case (gameOver newBoard) of -- check if 2048 in the list
				True -> putStrLn "\nYou Win!!!" >> recordGameHistory (show(round) ++ " (Won)")
				False -> if (newBoard == oriboard) -- check if the new and ori board are same
									then putStrLn "\nYour operation does not make any changes to the board.\nTry another input..." >> game oriboard round
									else do
										newBoard <- addNewValue newBoard
										case (gameContinue newBoard) of -- if the board is still movable
											True ->	do
												game newBoard (round + 1)
											False -> putStrLn "\nYou Lose..." >> recordGameHistory (show(round) ++ " (Lose)")