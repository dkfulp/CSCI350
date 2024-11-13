-- Getting Started with Haskell
-- Example program that creates a basic Haskell program that demonstrates:
-- 1. Main Function and Printing
-- 2. Basic Arithmetic
-- 3. Basic Functions

-- Imports modulus function for use on floating-point values
import Data.Fixed (mod')
-- Imports xor function for use in boolean algebra
import Data.Bits (xor)

-- PART 1 ****************************************
-- Declares main function as IO function 
-- this means it does not return a meaningful result 
-- instead operates with outside world
main :: IO ()
-- do keyword starts block where we can perform multiple
-- IO operations in the order they are written
main = do
    -- PART 2  ****************************************
    -- Use let keyword to assign values to variables
    let x = 5
    let y = 10
    -- Can use variables to calculate and display a result
    -- show function converts a value into a String for display purposes
    -- the ++ concatenates two strings together
    putStrLn ("5 + 10 = " ++ show (x + y))
    
    -- Can use single let for multiple variables
    let a = 30
        b = 20
        c = 10
    putStrLn ("(30 - 20) * 10 = " ++ show ((a - b) * c))

    -- Can use curly braces and semi-colons too
    let { d = 5; e = 13; f = 2 }
    -- div function is for integer division and truncates result
    -- mod function is for modulus result
    -- By putting function in `` allow us to use them with infix notation instead of prefix
    putStrLn ("(5 `div` 13) % 2 = " ++ show ((d `div` e) `mod` f)) -- Same as mod (div d e) f
    
    -- / is for floating-point division
    -- mod' function is for floating point modulus
    -- fromIntegral converts an integral (int) type to a general numeric type (float)
    putStrLn ("(5 / 13) % 2 = " ++ show ((fromIntegral d / fromIntegral e) `mod'` fromIntegral f))
    
    -- Boolean algebra works using standard notation
    let t = True
    let f = False -- New f shadows old f (old f still exists but inaccessible)
    putStrLn ("True && False = " ++ show (t && f))
    putStrLn ("True || False = " ++ show (t || f))
    putStrLn ("not False = " ++ show (not f))
    putStrLn ("not (True `xor` False) = " ++ show (not (t `xor` f)))

    -- Testing for equality works using standard notation
    let a = 10
    let b = 20
    putStrLn ("a == b: " ++ show (a == b))
    putStrLn ("a /= b: " ++ show (a /= b))
    putStrLn ("not (a == b): " ++ show (not(a == b)))
    
    
    -- PART 3  ****************************************
    -- Functions are normally prefix so we will assume this
    -- To call function write function name, a space, and 
    -- then parameters seperated by spaces
    putStrLn ("8 + 1 = " ++ show (succ 8))
    putStrLn ("min 3.4 3.2 = " ++ show (min 3.4 3.2))
    putStrLn ("max 3.4 3.2 = " ++ show (max 3.4 3.2))


    
    
    
    
    
    