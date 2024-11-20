-- Working with Higher Order Functions in Haskell
-- Example program that creates a basic Haskell program that demonstrates:
-- 1. Curried Functions
-- 2. Higher-Orderisms in Order
-- 3. Maps and Filters
-- 4. Lambdas 
-- 5. Folds and Scans
-- 6. Function Composition



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


-- By calling a function with too few parameters we can
-- create new functions on the fly. Look at the following:
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

-- Calling this function with 99 returns GT.
-- Calling compare 100 returns a function that takes a number
-- and compares it with 100. That's what we want so we can 
-- do the following:

compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100

-- The type declaration stays the same because compare 100 
-- returns a function and have a type Ord while calling it 
-- with 100 returns a Num Ord.


-- We can also partially apply infix functions too
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- Remember / 10 99 is the same as 10 / 99



-- PART 2 ****************************************
-- Functions can take functions as parameters and also return them
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Note: The ( ) around a -> a is mandatory this time as they
-- indicate that the first parameter is a function that takes 
-- something and returns the same thing.

-- Fact: If our function requires us to pass it a function that 
-- takes only one parameter, we can partially apply a function 
-- to the point where it takes only one parameter and then pass it


-- We can use higher order programming to implement a standard
-- library function that takes two lists and zips them using a 
-- provided function
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- First, look at the type declaration! 
-- The first parameter is a function that takes two things (a, b)
-- and produces a third thing (c). They don't have to be the 
-- same types but they can be.

-- The second and third parameters are lists and the result is 
-- also a list. The first has to be a list of a's because the 
-- joining function takes a's as it's first argument. Same for b's.


-- PART 3 ****************************************
-- Other iconic higher order function is the map or filter functions
-- map takes a function and a list and applies that function to 
-- every element in the list, producing a new list.
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- The type signature says it takes a function that takes an a 
-- and returns a b, a list of a's and returns a list of b's.

-- Note: This could have been done using list comprehension but
-- in some cases this approach is more readable.


-- The other iconic function is the filter function. It takes a 
-- predicate (T/F function) and a list of a's and returns the 
-- list of elements that satisfy the predicate.
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) 
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

-- If p x evaluates to True, element gets added, if it doesn't 
-- the element stays out.

-- Note: This could have been done using list comprehension but
-- in some cases this approach is more readable.

-- *********
-- FACT: MAP, FILTER, LIST COMPREHENSION are the bread and butter
-- of any functional programmers toolbox. They get the job done!
-- *********


-- PART 4 ****************************************
-- Lambdas are anonymous function that are used because we need
-- a function once and are made with the sole purpose of passing
-- them to a higher-order function.

-- To make a lambda, we write a \ (looks like greek letter) and 
-- then write the parameters, seperated by spaces. After that 
-- comes a -> and then the function body. We usually surround them
-- with (), because otherwise they extend all the way to the right
-- (See below for examples....)


-- Lambdas not surrounded by () extend all the way to the right.
-- Interestingly, due to currying, the following two are equivalent
addThree1 :: (Num a) => a -> a -> a -> a
addThree1 x y z = x + y + z

addThree2 :: (Num a) => a -> a -> a -> a
addThree2 = \x -> \y -> \z -> x + y + z

-- We use the first approach as it's more readable though.


-- PART 5 ****************************************
-- Similar to map and filter we have fold functions that 
-- take a binary function, a starting value, and a list to 
-- fold up. The result is a single value.
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- In this case, the binary function is called with the 
-- accumulator and the first (or last) element. This produces
-- a new accumulator, then this process is repeated for all.

-- Other fold options:
-- * foldr - Traverses list from right to left
-- * foldl1 /foldr1 - Works the same but don't need to specify
-- initial starting values

-- Note: Folds can be used to implement any function where you 
-- traverse a list once, element by element, and then return 
-- something based on that.


-- Scan is similar to fold but reports all intermediate 
-- accumulator states in the form of a list. There is also 
-- scanl1 and scanr1
scanSum :: (Num a) => [a] -> [a]
scanSum xs = scanl (\acc x -> acc + x) 0 xs

-- Notice that it returns a list of all states and not just
-- a single value anymore.


-- PART 6 ****************************************
-- To combine functions, we can also use the . operator to 
-- combine functions just like in math. Using this operator 
-- allows us to write in what is called pointfree (pointless) style
oddSquareSum1 :: Integer
oddSquareSum1 = sum (takeWhile (<10000) (filter odd (map (^2) [1..]))) 

oddSquareSum2 :: Integer
oddSquareSum2 = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

-- Both of the above work the same way but one use the point style approach

-- Note: The $ above is the function applicator operator and applies 
-- a function at the lowest precedence level. So it will be done last.


main :: IO ()
main = do
    -- PART 1 ****************************************
    putStrLn(show( compareWithHundred 99) )
    putStrLn(show( compareWithHundred' 99) )
    
    putStrLn(show( divideByTen 99) )


    -- PART 2 ****************************************
    putStrLn(show( applyTwice divideByTen 99) )
    putStrLn(show( applyTwice (3:) [1] ) )
    putStrLn(show( applyTwice (+3) 10 ) )
    putStrLn(show( applyTwice (++ " HAHA") "HEY" ) )


    -- Apply twice is a function that takes a function that 
    -- only has one parameter but we can use this with multThree
    -- by partially applying multThree until it only takes one parameter
    putStrLn(show( applyTwice (multThree 2 2) 9) )
    -- So this becomes 4 * 4 * 9 = 144
    
    
    -- This new function works similar to zip but the joining 
    -- function provides more flexibility.
    putStrLn(show( zipWith' (+) [4,2,5,6] [2,6,2,3] ) )
    putStrLn(show( zipWith' max [4,2,5,6] [2,6,2,3] ) )
    putStrLn(show( zipWith' (++) ["foo ", "bar "] ["fighters", "hoppers"] ) )
    putStrLn(show( zipWith' (*) (replicate 5 2) [1..] ) )


    -- PART 3 ****************************************
    putStrLn(show( map' (+3) [1,5,3,1,6] ) )
    putStrLn(show( map' (replicate 3) [3..6] ) )
    putStrLn(show( map' (++ "!") ["BIFF", "BANG", "POW"] ) )
    putStrLn(show( map' (replicate 3) [3..6] ) )
    putStrLn(show( map' (map' (^2)) [[1,2],[3,4,5,6],[7,8]] ) )


    putStrLn(show( filter' (>3) [1,5,3,1,6] ) )
    putStrLn(show( filter' (==3) [1,5,3,1,6] ) )
    putStrLn(show( filter' even [1,5,3,1,6] ) )


    -- PART 4 ****************************************
    let r = zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
    putStrLn(show( r ) )
    let r2 = map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
    putStrLn(show( r2 ) )
    
    
    -- PART 5 ****************************************
    putStrLn(show( sum' [1,4,5,6,7] ) )
    
    putStrLn(show( scanSum [1,4,5,6,7] ) )


    -- PART 6 ****************************************
    putStrLn(show( oddSquareSum1 ) )
    
    putStrLn(show( oddSquareSum2 ) )
