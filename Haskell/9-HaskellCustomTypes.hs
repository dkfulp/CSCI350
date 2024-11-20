-- Working with Custom Types and Typeclasses in Haskell
-- Example program that creates a basic Haskell program that demonstrates:
-- 1. Record Syntax
-- 2. Type Parameters
-- 3. Derived Instances


-- PART 1 ****************************************
-- We want to create a new data type that records information 
-- on a person. We want to keep track of their first name, last
-- name, age, height, phone number, and favorite ice-cream flavor

-- To do this we do the following:
data Person = Person String String Int Float String String deriving (Show)

-- We can create functions to get separate info from a person 
firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname

lastName :: Person -> String
lastName (Person _ lastname _ _ _ _) = lastname

age :: Person -> Int
age (Person _ _ age _ _ _) = age

height :: Person -> Float
height (Person _ _ _ height _ _) = height

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ number _) = number

flavor :: Person -> String
flavor (Person _ _ _ _ _ flavor) = flavor

-- This is a lot of work, there is an easier way to do this
data Person' = Person' { firstName' :: String
                     , lastName' :: String
                     , age' :: Int
                     , height' :: Float
                     , phoneNumber' :: String
                     , flavor' :: String
                     } deriving (Show) 
-- This does the same thing as above all in one!


-- PART 2 ****************************************
-- Above we saw a value constructor take values to produce a 
-- new person value.

-- We can use a similar approach to provide types as parameters
-- allowing us to create type constructors.

data Maybe a = Nothing | Just a

-- In this case, the a is the type parameter. Because a type 
-- parameter is involved Maybe is a type constructor. Depending 
-- on the data type, this type constructor can end up producing 
-- a type of Maybe int, Maybe Person, ....

-- We saw another type constructor before with the list type. 
-- In this case we can have [Int], [Char], [String], ....

-- In both cases we cannot have just a Maybe or just a list 
-- They need to have a type associated with them.


-- Using this approach we can create a vector data type that 
-- has type parameterization 
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n


-- PART 3 ****************************************
-- Haskell can automatically make our type an instance of 
-- any of the following typeclasses: Eq, Ord, Enum, Bounded
-- Show, and Read.

-- By being a member of Eq, we get comparison capabilities
data Car = Car { company :: String
                , model :: String
                , year :: Int
                } deriving (Eq)
                
-- By being a member of Show and Read, we get the ability 
-- to convert to or from strings
data Carv2 = Carv2 { companyv2 :: String
                    , modelv2 :: String
                    , yearv2 :: Int
                    } deriving (Eq, Show, Read)

-- By being a member of Ord, we get the ability to order 
-- potential values of a type 
-- data Bool = False | True deriving (Ord)
-- In this case, since True comes second, it is considered
-- greater than False.

-- By being a member of Bounded and Enum, we get an enumeration
-- data type with a set number of values
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday 
           deriving (Eq, Ord, Show, Read, Bounded, Enum)



main :: IO ()
main = do
    -- PART 1 ****************************************
    -- We can create an instance of our custom type 
    let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
    putStrLn( show ( guy ) )
    
    -- We can call the functions we created for our custom type
    putStrLn( firstName guy  )
    putStrLn( lastName guy  )
    putStrLn( show ( age guy ) )
    putStrLn( show ( height guy ) )
    putStrLn( flavor guy )
    
    -- We can create an instance of our new custom type 
    let guy2 = Person' "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
    putStrLn( show ( guy2 ) )
    
    -- We can call the functions we created for our custom type
    putStrLn( firstName' guy2  )
    putStrLn( lastName' guy2  )
    putStrLn( show ( age' guy2 ) )
    putStrLn( show ( height' guy2 ) )
    putStrLn( flavor' guy2 )


    -- PART 2 ****************************************
    -- We can call out vector type with different inputs
    putStrLn( show (Vector 3 5 8 `vplus` Vector 9 2 8 ) )
    putStrLn( show (Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3) )
    putStrLn( show (Vector 3 9 7 `vectMult` 10 ) )
    putStrLn( show (Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0 ) )
    putStrLn( show (Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4) ) )


    -- PART 3 ****************************************
    -- By being a member of Eq, we get the following
    let c1 = Car {company = "Ford", model = "Mustang", year = 1967}
    let c2 = Car {company = "Toyota", model = "Camry", year = 2000}

    putStrLn( show (c1 == c2)  )
    
    let inventory = [c1, c2]
    putStrLn( show (c1 `elem` inventory)  )

    -- By being a member of Show and Read, we get the following
    let c3 = Carv2 {companyv2 = "Ford", modelv2 = "Mustang", yearv2 = 1967}

    putStrLn( show (c3)  )
    putStrLn( "Car 3 is " ++ show (c3)  )
    
    let c4 = read "Carv2 {companyv2 = \"Toyota\", modelv2 = \"Camry\", yearv2 = 2000}" :: Carv2
    putStrLn( show (c4)  )

    -- By being a member of Ord, we get the following
    putStrLn( show (True `compare` False)  )

    -- By being a member of Enum and Bounded, we get the following
    putStrLn( show (Wednesday)  )
    let d1 = read "Saturday" :: Day
    putStrLn( show (d1)  )
    putStrLn( show (d1 == Wednesday)  )
    putStrLn( show (d1 `compare` Monday)  )
    putStrLn( show (minBound :: Day)  )
    putStrLn( show (maxBound :: Day)  )
    putStrLn( show (succ Monday)  )
    putStrLn( show (pred Saturday)  )
    putStrLn( show ([minBound .. maxBound] :: [Day])  )

