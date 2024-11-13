-- Working with Function Syntax Part 1 in Haskell
-- Example program that creates a basic Haskell program that demonstrates:
-- 1. Defining a Pattern Matching Function
-- 2. Using Pattern Matching Functions with Tuples
-- 3. Using Pattern Matching Function with Lists


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


-- PART 2 ****************************************
-- We can use pattern matching to take two tuples and add them
--addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
--addVectors a b = (fst a + fst b, snd a + snd b)

-- This works but there is a better way to go about it
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
-- Using this second approach, we have a catch-all pattern

-- We can also use pattern matching to make fst, snd functions
-- for larger tuples as well
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- The _ above means that we don't care what that part is


-- PART 3 ****************************************
-- Pattern Matching also works with Lists
-- We can match on any empty list or pattern that involves :
-- and the empty list. ( Remember [1,2,3] === 1:2:3:[] )
-- This head tail pattern is used often with recursive functions

-- We can use this idea to make our own implementation of head
head' :: [a] -> a
head' [] = error "Can't call head on an empty list!"
head' (x:_) = x

-- We still need to use () to bind several variables, even if 
-- one of them is just an _

-- We can go a step further with this approach below
tell :: (Show a) => [a] -> String
tell [] = "List is empty"
tell (x:[]) = "List has one element: " ++ show x
tell (x:y:[]) = "List has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "List is long. First two elements are: " ++ show x ++ " and " ++ show y
-- This function is safe as we cover every case possible

-- We can also implement our own length and sum functions
-- using the same pattern matching approaches
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs
-- Both of these operate similar to the factorial function

-- We can also use the same : list approach with the @
-- sign to have quick access to the entire string
-- This is known as an "as pattern"
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
-- With this approach, we can use all to display the entire content of the string


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


    -- PART 2 ****************************************
    let x = (10, 3)
    let y = (4, 11)
    putStrLn("(10, 3) + (4, 11) = " ++ show(addVectors x y))

    let z = ("John", 11, 35)
    putStrLn(show(first z) ++ " " ++ show(second z) ++ " " ++ show(third z))

    
    -- PART 3 ****************************************
    let c = [] :: [Int] 
    -- Not putting Int leaves this ambiguous 
    -- and it wont work in our functions we make
    -- (The type needs to implement head and show)
    let d = [1..5]
    let e = "Goodbye"
    --putStrLn("Head of C: " ++ show(head' c))
    -- This line causes error to occur
    putStrLn("Head of D: " ++ show(head' d))
    putStrLn("Head of E: " ++ show(head' e))

    putStrLn("Story of C: " ++ show(tell c))
    putStrLn("Story of D: " ++ show(tell d))
    putStrLn("Story of E: " ++ show(tell e))

    putStrLn("Length of D: " ++ show(length' d))
    putStrLn("Sum of D: " ++ show(sum' d))
    
    putStrLn(show(capital e))