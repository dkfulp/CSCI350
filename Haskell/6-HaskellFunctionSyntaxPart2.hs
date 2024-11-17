-- Working with Function Syntax Part 2 in Haskell
-- Example program that creates a basic Haskell program that demonstrates:
-- 1. Defining Functions with Guards expressions
-- 2. Defining Functions with Where expressions
-- 3. Defining Functions with Let In expressions
-- 4. Defining Functionw with Recursion


-- PART 1 ****************************************
-- Where patterns make sure a value conforms to a pattern,
-- guarded are a way of testing whether some property of a 
-- value are true or false (very similar to if statements 
-- but are much easier to read)
salaryTell :: (RealFloat a) => a -> String
salaryTell salary
    | salary <= 50000.00 = "You're lower class"
    | salary <= 90000.00 = "You're middle class"
    | salary <= 150000.0 = "You're upper class"
    | otherwise   = "You're extremely rich!"

-- Guards use the pipe symbol as seen above and are indented
-- a bit and lined up. If condition is True, use function body
-- that follows = sign. If False, drop through to next guard. 
-- Often, the last guard is otherwise which acts as a catch all

-- If all guards evaluate to False and there is no otherwise,
-- evaluation falls through to the next pattern. That's how 
-- patterns and guards play nicely together. If no guard or pattern 
-- matches are found, an error is thrown instead


-- We can use guards with as many parameters as we want too
salaryTell' :: (RealFloat a) => a -> a -> String
salaryTell' salary bonus
    | salary + bonus <= 50000.00 = "You're lower class"
    | salary + bonus <= 90000.00 = "You're middle class"
    | salary + bonus <= 150000.0 = "You're upper class"
    | otherwise   = "You're extremely rich!"

-- We can use guard expressions to recreate the max function
max' :: (Ord a) => a -> a -> a
max' a b 
    | a > b     = a
    | otherwise = b

-- We can also write guard functions on one line if we prefer:
-- max' :: (Ord a) => a -> a -> a
-- max' a b | a > b = a | otherwise = b


-- We can also create our own comparison function
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT
-- Just like how we can call functions infix using backticks
-- We can define them this way as well if it is easier for us
-- to read them


-- PART 2 ****************************************
-- In the previous section, we found ourselves repeating math
-- in our guards pretty often, we can reduce the need for this
-- by using the where keyword
salaryTellv2 :: (RealFloat a) => a -> a -> String
salaryTellv2 salary bonus
    | total <= 50000.00 = "You're lower class"
    | total <= 90000.00 = "You're middle class"
    | total <= 150000.00 = "You're upper class"
    | otherwise   = "You're extremely rich!"
    where total = salary + bonus


-- By putting the where keyword after the guards, we can define 
-- several names or functions internally so we don't have to 
-- repeat ourselves. This makes the code more managable.
salaryTellv3 :: (RealFloat a) => a -> a -> String
salaryTellv3 salary bonus
    | total <= lower = "You're lower class"
    | total <= middle = "You're middle class"
    | total <= upper = "You're upper class"
    | otherwise   = "You're extremely rich!"
    where   total = salary + bonus
            lower = 50000.00
            middle = 90000.00
            upper = 150000.00

-- The names we define in the where section are only visible 
-- to that function so this does not pollute the namespace. 
-- This includes function bodies of different patterns.

-- Note: The alignment here matters so Haskell doesn't get confused


-- We can also use the where bindings to pattern match!
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname  

-- We could have done this pattern matching in the function
-- parameters but this just shows it's possible to do

-- Lastly, we can use it with list comprehension as well
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2


-- PART 3 ****************************************
-- Similar to where bindings, the let bindings allow us to bind
-- variables at the end of a function so that the whole function 
-- can see them, including all the guards
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea

-- The names defined in the let part are accessible to the 
-- expression after the in part.
-- But couldn't we have just used the where binding?

-- The difference is that let bindings are expressions themselves
-- while where bindings are just syntactic constructs.

-- Rememeber that we can do the following with if statements
-- ghci> 4 * (if 10 > 5 then 10 else 0) + 2
-- Output: 42

-- We can do the same thing with let bindings
-- ghci> 4 * (let a = 9 in a + 1) + 2
-- Output: 42

-- We can use let in many different ways
-- ghci> [let square x = x * x in (square 5, square 3, square 2)]
-- Output: [(25,9,4)]

-- ghci> (let (a,b,c) = (1,2,3) in a+b+c) * 100
-- Output: 600

-- ghci> (let (a,b,c) = (1,2,3) in a+b+c) * 100
-- Output: 600

-- Lastly, we can use it with list comprehension as well
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- We don't just use let bindings all the time since they can't 
-- be used across guards. Also some people prefer where bindings
-- as the function name is closer to the function they are being 
-- used in.


-- PART 4 ****************************************
-- A key part of many Haskell functions is using recursion
-- We can do this using the ideas we just covered
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) 
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs
    
-- Another way to write the same function
maximumv2 :: (Ord a) => [a] -> a
maximumv2 [] = error "maximum of empty list"
maximumv2 [x] = x
maximumv2 (x:xs) = max x (maximumv2 xs)

-- Some other recursive functions we can look at
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted



main :: IO ()
main = do
    -- PART 1 ****************************************
    -- We can call our guarded function like normal
    -- In this case, it will tell us that we are middle
    -- class as it falls through the first and lands in 
    -- the second
    let a = 100000.00
    putStrLn( salaryTell a )

    -- We can call our new guarded method that takes 
    -- multiple parameters this time by adding another one
    let b = 30000.00
    putStrLn( salaryTell' a b )

    -- We can call our newly made max function
    putStrLn( show( max' a b ) )

    -- We can use our newly made comparison function
    putStrLn( show( a `myCompare` b ) )


    -- PART 2 ****************************************
    -- We can call our new function that uses wheres
    putStrLn( salaryTellv2 a b )
    putStrLn( salaryTellv3 a b )

    -- We can call our initials function
    putStrLn ( initials "Dakota" "Fulp" )
    
    -- We can call our calculate BMI function (kg, m)
    putStrLn ( show ( calcBmis [(113.4, 1.83), (85, 1.90)] ) )

    
    -- PART 3 ****************************************
    -- We can call our cylinder function
    putStrLn ( show ( cylinder 3 10 ) )

    -- We can call our calculate BMI function (kg, m)
    putStrLn ( show ( calcBmis' [(113.4, 1.83), (85, 1.90)] ) )


    -- PART 4 ****************************************
    -- We can call our recursion function 
    putStrLn ( show ( maximum' [3, 10, 11, 32] ) )
    putStrLn ( show ( maximumv2 [3, 10, 11, 32] ) )

    putStrLn ( show ( replicate' 10 3 ) )
    
    putStrLn ( show ( zip' ["Jim", "John", "Anne"] [18, 25, 21] ) )

    putStrLn ( show ( quicksort [3, 10, 11, 32, 1, 50] ) )