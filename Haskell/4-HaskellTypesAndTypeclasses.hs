-- Working with Types and Typeclasses in Haskell
-- Example program that creates a basic Haskell program that demonstrates:
-- 1. Declaring and checking variable types
-- 2. Declaring and checking function types
-- 3. Working with Typeclasses

-- Needed to use typeOf command in Haskell
import Data.Typeable

-- PART 2 ****************************************
-- When declaring functions we can also give them
-- an explicit type declaration.
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']] 

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

-- When a function takes multiple parameters we can
-- explicitly set each of those too
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z



main :: IO ()
main = do
    -- PART 1 ****************************************
    -- When declaring variables we can set type
    -- Not nessecary but considered good practice
    let a = 30 :: Int -- Represents whole numbers (Has max value)
    putStrLn(show(typeOf a) ++ " " ++ show(a))
    let b = 123456789 :: Integer -- Not bound and can be used to represent really big numers
    putStrLn(show(typeOf b) ++ " " ++ show(b))
    let c = 3.14 :: Float -- Represents single precision floating point
    putStrLn(show(typeOf c) ++ " " ++ show(c))
    let d = 3.1415926 :: Double -- Represents double precision floating point
    putStrLn(show(typeOf d) ++ " " ++ show(d))
    let e = 'a' :: Char -- Represents character
    putStrLn(show(typeOf e) ++ " " ++ show(e))
    let f = False :: Bool -- Represents boolean value
    putStrLn(show(typeOf f) ++ " " ++ show(f))
    
    -- Note 1: lists are not types but are a collection of 
    -- a given type
    -- Note 2: each size of tuple is its own type so a 
    -- 2-tuple (t2) is different from a 3-tuple (t3)
    
    
    -- PART 2 ****************************************
    -- When declaring functions we can also give them
    -- an explicit type declaration.
    putStrLn(show(typeOf removeNonUppercase) ++ " " ++ removeNonUppercase "HaHaHaHaHa")
    putStrLn(show(typeOf factorial) ++ " " ++ show (factorial 50))
    putStrLn(show(typeOf circumference) ++ " " ++ show (circumference 4.0))
    putStrLn(show(typeOf circumference') ++ " " ++ show (circumference' 4.0))
    putStrLn(show(typeOf addThree) ++ " " ++ show(addThree 10 11 12))

    -- We can also check the types of built-in functions
    -- Note: due to the polymorphic nature of these functions, 
    -- we can only check them through GHCI terminal
    -- For instance:
    -- ghci> :t head 
    -- head :: [a] -> a
    
    -- Remembering that types are capitalized, a is not.
    -- In this case, a is a type variable making head 
    -- a polymorphic function.
    
    -- This allows us to write generic functions as long
    -- as they do not use any specific behavior of types in them
    
    -- Another example:
    -- :t fst
    -- fst :: (a, b) -> a
    
    -- In this case, it only works on a 2-tuple type 
    -- and that they return type will be the same as 
    -- the first element
    
    -- Note we cannot actually do typeOf head / fst 
    -- as the type must be set at runtime. This is 
    -- not possible with a direct polymorphic function.
    
    
    -- PART 3 ****************************************
    -- Typeclasses defined an interface behavior of 
    -- sorts that all types within the typeclass implement
    -- Note: Not Classes, closer to interfaces
    
    -- We can check the type signature of == function
    -- Note: == is a function, just like +,-,*,/.
    -- If function consists of only special characters it is
    -- an infix function by default.
    
    -- :t (==)
    -- (==) :: Eq a => a -> a -> Bool

    -- In this case, we see the => symbol. 
    -- Everything before it is a class constraint 
    -- Reading this we see:
    --  *Equality function takes any two values of same type
    --  *Returns a Bool type 
    --  *Types of two values must be members of Eq class
    
    -- The Eq typeclass provides interface for testing Equality
    -- Any type testing equality should be a member of Eq 
    
    -- Some basic typeclasses:
    -- Eq - Used for supporting equality testing (uses == and /=_)
    -- Ord - Used for types that have an ordering
    -- Show - Used for types that can be represented as a string
    -- Read - Used for opposite by taking string and returning value
        -- For instance,
        -- ghci> read "True" || False
        -- True
        -- ghci> read "8.2" + 3.8
        -- 12.0
        -- ghci> read "5" - 2
        -- 3
        -- ghci> read "[1,2,3,4]" ++ [3]
        -- [1,2,3,4,3]
        
        -- In read's case it will convert the type to one that can 
        -- work with the next part.
    -- Enum - Used for sequential ordered types that can be enumerated
    -- Bounded - Used for types taht have an upper and lower Bounded
    -- Num - Used for types that act like numbers (all types)
    -- Integral - Used for types that are whole numbers
    -- Floating - Used for types that are real numbers
    
    -- To be in some type classes you need to be friends with 
    -- other typeclasses as well
    -- To join Num, a type must be friends with Show and Eq




    
