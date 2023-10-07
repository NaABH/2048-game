module GameInterface where

        import GameHistoryProcessing

        -- menu page
        menu :: IO String
        menu = putStrLn "\n=================================================================" >>
                putStrLn "||                         2048 : Menu                         ||" >>
                putStrLn "=================================================================" >>
                putStrLn "1. Start playing  " >>
                putStrLn "2. How to play  " >>
                putStrLn "3. GamePlay History  " >>
                putStrLn "4. Exit                            " >>
                putStrLn "=================================================================" >>
                putStr "Enter your selection: " >>
                getLine

        -- instruction (How to play) page
        instruction :: IO String
        instruction = putStrLn "\n=================================================================" >>
                        putStrLn "||                        2048 : Rules                         ||" >>
                        putStrLn "=================================================================" >>
                        putStrLn "\n1. There are 16 grids in the 2048 game, and the initial numbers are composed of 2 or 4 at the beginning." >>
                        putStrLn "2. Enter w, a, s or d to make all grids move in that direction. \n  'w' --> move up \n  'a' --> move left \n  's' --> move down \n  'd' --> move right" >>
                        putStrLn "3. When two grids with the same number collide, they will fuse together." >>
                        putStrLn "4. Every time when the grids move, a number grid will be randomly refreshed in the blank space." >>
                        putStrLn "5. When the interface is immobile (when all grids are filled with numbers), the game ends. \n   When the maximum number in the interface is 2048, the player wins." >>
                        putStrLn "\n=================================================================" >>
                        putStrLn "Press 'Enter' to go back to Menu Page" >>
                        getLine

        -- history page
        history :: IO String
        history = putStrLn "\n=================================================================" >>
                        putStrLn "||                        2048 : History                       ||" >>
                        putStrLn "=================================================================" >>
                        displayAllRecords >>
                        putStrLn "\n=================================================================" >>
                        putStrLn "Press 'Enter' to go back to Menu Page" >>
                        getLine