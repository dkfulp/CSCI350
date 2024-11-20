-- Working with Higher Order Functions in Haskell
-- Example program that creates a basic Haskell program that demonstrates:
-- 1. Inputting user input
-- 2. Checking it against different cases
-- 3. Performing actions and using recursion to continue the loop

-- Function to perform a computation based on the user's choice
performComputation :: Int -> Int -> Int
performComputation 1 n = n * 2  -- Double
performComputation 2 n = n * n  -- Square
performComputation 3 n = n + 1  -- Increment
performComputation _ n = n      -- Default case: return the number unchanged

-- Main recursive loop
mainLoop :: IO ()
mainLoop = do
    putStrLn "Enter a number:"
    numberInput <- getLine
    let maybeNumber = reads numberInput :: [(Int, String)]
    case maybeNumber of
        [(number, "")] -> do
            putStrLn "Choose an operation by entering the corresponding number:"
            putStrLn "1. Double the number"
            putStrLn "2. Square the number"
            putStrLn "3. Increment the number by 1"
            operationInput <- getLine
            let maybeOperation = reads operationInput :: [(Int, String)]
            case maybeOperation of
                [(operation, "")] | operation `elem` [1, 2, 3] -> do
                    let result = performComputation operation number
                    putStrLn $ "Result: " ++ show result
                    putStrLn "Do you want to perform another computation? (yes/no):"
                    continue <- getLine
                    if continue == "yes"
                        then mainLoop
                        else putStrLn "Goodbye!"
                _ -> putStrLn "Invalid operation. Please enter 1, 2, or 3." >> mainLoop
        _ -> putStrLn "Invalid input. Please enter a valid number." >> mainLoop

-- Entry point
main :: IO ()
main = do
    putStrLn "Welcome to the number computation program!"
    mainLoop
