wordcheck :: String -> String -> Int -> String
wordcheck _ _ n 
    | n >= 5 = ""
wordcheck t i n
    | (t !! n) == (i !! n) = 'G' : wordcheck t i (n+1)
    | (i !! n) `elem` t    = 'Y' : wordcheck t i (n+1)
    | otherwise            = 'W' : wordcheck t i (n+1)

main :: IO ()
main = do
    putStrLn "Give me a target word:"
    target <- getLine
    putStrLn "\ESC[2J"
    putStrLn "Try to Guess the word!"
    guessLoop target 1

guessLoop :: String -> Int -> IO ()
guessLoop target round
    | round > 6 = putStrLn "You lose"
    | otherwise = do
        input <- getLine
        if length input /= length target
            then do
                putStrLn "Input must be the same length as the target word."
                guessLoop target round
            else if input == target
                then putStrLn $ "You have solved it on round " ++ show round ++ "!"
                else do
                    putStrLn (wordcheck target input 0)
                    putStrLn $ "Try " ++ show (round + 1)
                    guessLoop target (round + 1)