-- Working with Functions in Haskell
-- Example program that creates a basic Haskell program that demonstrates:
-- 1. Creating Functions
-- 2. Making decisions in functions

main :: IO ()
main = do
    -- PART 1 ****************************************
    -- We can create functions inline and store them in variables
    let doubleMe x = x + x
    -- We can then recall these functions as many times as needed
    putStrLn ("10 + 10 = " ++ show(doubleMe 10))
    putStrLn ("30 + 30 = " ++ show(doubleMe 30))
    
    -- These functions can take multiple parameters
    let doubleUs x y = x*2 + y*2
    putStrLn ("10*2 + 20*2 = " ++ show(doubleUs 10 20))
    putStrLn ("30*2 + 11*2 = " ++ show(doubleUs 30 11))
    
    -- These functions can use other functions too
    -- Also known as function composition
    let doubleUsV2 x y = doubleMe x + doubleMe y
    putStrLn ("10*2 + 20*2 = " ++ show(doubleUsV2 10 20))
    putStrLn ("30*2 + 11*2 = " ++ show(doubleUsV2 30 11))
    
    -- We can also make decisions in these functions
    -- Unlike imperative languages, the else part is a must
    -- Can be written like below or on single line
    let doubleSmallNum x = if x > 100 
                            then x 
                            else x * 2
    putStrLn ("10 + 10 = " ++ show(doubleSmallNum 10))
    putStrLn ("30 + 30 = " ++ show(doubleSmallNum 300))
    
    -- When a function takes no parameters we call it a definition
    -- Note that functions cannot start with a capital letter
    let nikolaTesla = "Hello there from Nikola Tesla!"
    putStrLn (nikolaTesla)
    
    -- Note that all "variables" are also defined the same
    -- way so in reality there are no variables in Haskell
    -- just functions that return a specified value
    