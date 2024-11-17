-- Working with Higher Order Functions in Haskell
-- Example program that creates a basic Haskell program that demonstrates:
-- 1. Curried Functions
-- 2. Higher-Orderisms in Order
-- 3. Maps and Filters
-- 4. Lambdas 
-- 5. Folds and Scans
-- 6. Function Application with $
-- 7. Function Composition



-- PART 1 ****************************************
-- Every function only takes one parameter, so how have they
-- been taking multiple then? It's a clever trick called currying 

-- The following two are the same thing
-- ghci> max 4 5
-- ghci> (max 4) 5

-- The space between two things is function application.
-- The space acts like an operator and has the highest precedence

-- Looking at max we see max :: (Ord a) => a -> a -> a
-- This can also be written as max :: (Ord a) => a -> (a -> a)
-- This reads as max takes an a and returns a function that 
-- takes an a and returns an a.


-- Let's look at this function
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- What really happens when given multThree 3 5 9?
-- 1st, multThree y z = 3 * y * z
-- 2nd, multThree z = 3 * 5 * z
-- Finally, 9 is appiled to the function to get 135


-- We can also partially use a function to create new 
-- functions on the fly 
-- ghci> let multTwoWithNine = multThree 9
-- ghci> multTwoWithNine 2 3
-- Output: 54

-- ghci> let multWithEighteen = multTwoWithNine 2
-- ghci> multWithEighteen 10
-- Output: 180



-- PART 2 ****************************************


-- PART 3 ****************************************


-- PART 4 ****************************************



main :: IO ()
main = do
    -- PART 1 ****************************************
   

    -- PART 2 ****************************************


    -- PART 3 ****************************************


    -- PART 4 ****************************************
