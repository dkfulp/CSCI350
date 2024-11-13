-- Working with Function Syntax in Haskell
-- Example program that creates a basic Haskell program that demonstrates:
-- 1. Defining a Pattern Matching Function

-- PART 1 ****************************************
-- When defining a function, we can set separate bodies for 
-- different patterns. We can pattern match on any data type

-- Patterns checked from top to bottom and when pattern found, 
-- function body is used
-- If input is 7, return lucky string, anything else return sorry
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!" 

-- If the first pattern does not match, it falls through to 
-- the next pattern and so on. The order matters here! 
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"
-- We need to make sure we match all patterns if we use this approach

-- We can use this approach to implement recursive functions
-- like this factorial function.
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)



main :: IO ()
main = do
    -- PART 1 ****************************************
    let a = 3
    putStrLn(lucky a)
    let b = 7
    putStrLn(lucky b)
    
    putStrLn(sayMe a)
    putStrLn(sayMe b)

    putStrLn("Factorial of 7: " ++ show(factorial b))




    
