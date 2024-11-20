-- Working with Modules in Haskell
-- Example program that creates a basic Haskell program that demonstrates:
-- 1. Loading Modules
-- 2. List Module
-- 3. Char Module
-- 4. Map Module 
-- 5. Creating Custom Module



-- PART 1 ****************************************
-- To import modules do the following (use :m in ghci)
import Data.List as L
import Data.Char as C
import Data.Map as M

-- If you want to import just a subset you can do:
-- import Data.List (nub, sort)

-- If you want to import all but a subset you can do:
-- import Data.List hiding (nub)

-- Another way to deal with naming clases is to do 
-- qualified imports as follows:
-- import qualified Data.Map 

-- This makes it so that if we want to reference Data.Map's 
-- filter function we have to do Data.Map.filter, whereas 
-- filter refers to normal fitlter method.

-- To shorten this we can use aliasing
-- import qualified Data.Map as M 


-- PART 4 ****************************************
-- We can create functions to help us find a key
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) = if key == k
                            then Just v
                            else findKey key xs


-- PART 5 ****************************************
-- To create our own module, we would place the following 
-- code in a file called Geometry.hs:

-- module Geometry
-- ( sphereVolume
-- , sphereArea
-- , cubeVolume
-- , cubeArea
-- , cuboidArea
-- , cuboidVolume
-- ) where

-- sphereVolume :: Float -> Float
-- sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

-- sphereArea :: Float -> Float
-- sphereArea radius = 4 * pi * (radius ^ 2)

-- cubeVolume :: Float -> Float
-- cubeVolume side = cuboidVolume side side side

-- cubeArea :: Float -> Float
-- cubeArea side = cuboidArea side side side

-- cuboidVolume :: Float -> Float -> Float -> Float
-- cuboidVolume a b c = rectangleArea a b * c

-- cuboidArea :: Float -> Float -> Float -> Float
-- cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

-- rectangleArea :: Float -> Float -> Float
-- rectangleArea a b = a * b

-- Then we would import Geometry just as we have done before.


main :: IO ()
main = do
    -- PART 2 ****************************************
    -- Data.List is all about lists and list functionality
    
    -- Intersperse function
    putStrLn( show (intersperse '.' "COASTAL" ))
    
    -- Intercalate function
    putStrLn( show (intercalate " " ["Hey", "there", "guys"] ))

    -- Transpose function
    putStrLn( show (transpose [[1,2,3],[4,5,6],[7,8,9]] ))

    -- concat function 
    putStrLn( show (concat ["foo", "bar", "car"] ))
    putStrLn( show (concat [[1,2,3],[4,5,6],[7,8,9]] ))

    -- concatMap function 
    putStrLn( show (concatMap (replicate 4) [1..3] ))

    -- any function 
    putStrLn( show (any (==4) [2,3,4,5,6] ))

    -- all function 
    putStrLn( show (all (>=4) [2,3,4,5,6] ))

    -- iterate function
    putStrLn( show (L.take 3 $ iterate (++ "haha") "haha" ))
    
    -- splitAt function
    putStrLn( show (L.splitAt 3 "heyman" ))

    -- takeWhile function 
    putStrLn( show (takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1] ))

    -- dropWhile function 
    putStrLn( show (dropWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1] ))

    -- break function
    putStrLn( show (break (==4) [1,2,3,4,5,6,7] ))
    
    -- span function 
    putStrLn( show (span (/=4) [1,2,3,4,5,6,7] ))

    -- group function
    putStrLn( show (group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7] ))

    -- inits function
    putStrLn( show (inits "Hello" ))

    -- tails function 
    putStrLn( show (tails "Hello" ))

    -- find function 
    putStrLn( show (find (>4) [1,2,3,4,5,6] ))
    putStrLn( show (find (>9) [1,2,3,4,5,6] ))

    -- elemIndex function 
    putStrLn( show (elemIndex 'e' "Hello" ))

    -- findIndex function
    putStrLn( show (L.findIndex (=='e') "Hello" ))
    putStrLn( show (L.findIndex (==4) [5,3,2,1,6,4] ))

    -- nub function (removes duplicates)
    putStrLn( show (nub [1,2,3,4,3,2,1,2,3,4,3,2,1] ))


    -- PART 3 ****************************************
    
    -- isControl checks whether a character is a control character.
    -- isSpace checks whether a character is a white-space characters. That includes spaces, tab characters, newlines, etc.
    -- isLower checks whether a character is lower-cased.
    -- isUpper checks whether a character is upper-cased.
    -- isAlpha checks whether a character is a letter.
    -- isAlphaNum checks whether a character is a letter or a number.
    -- isPrint checks whether a character is printable. Control characters, for instance, are not printable.
    -- isDigit checks whether a character is a digit.
    -- isOctDigit checks whether a character is an octal digit.
    -- isHexDigit checks whether a character is a hex digit.
    -- isLetter checks whether a character is a letter.
    -- isMark checks for Unicode mark characters. Those are characters that combine with preceding letters to form latters with accents. Use this if you are French.
    -- isNumber checks whether a character is numeric.
    -- isPunctuation checks whether a character is punctuation.
    -- isSymbol checks whether a character is a fancy mathematical or currency symbol.
    -- isSeparator checks for Unicode spaces and separators.
    -- isAscii checks whether a character falls into the first 128 characters of the Unicode character set.
    -- isLatin1 checks whether a character falls into the first 256 characters of Unicode.
    -- isAsciiUpper checks whether a character is ASCII and upper-case.
    -- isAsciiLower checks whether a character is ASCII and lower-case.
    
    -- generalCategory returns the category a character would fall into
    
    -- toUpper converts a character to upper-case. Spaces, numbers, and the like remain unchanged.
    -- toLower converts a character to lower-case.
    -- toTitle converts a character to title-case. For most characters, title-case is the same as upper-case.
    -- digitToInt converts a character to an Int. To succeed, the character must be in the ranges '0'..'9', 'a'..'f' or 'A'..'F'.
    -- intToDigit is the inverse function of digitToInt. It takes an Int in the range of 0..15 and converts it to a lower-case character.
    -- ord and chr functions convert characters to their corresponding numbers and vice versa
    

    -- PART 4 ****************************************
    -- Association lists are lists that are used to store key-value
    -- pairs when ordering doesn't matter
    let phoneBook = [("betty","555-2938")
                    ,("bonnie","452-2928")
                    ,("patsy","493-2928")
                    ,("lucille","205-2928")
                    ,("wendy","939-8282")
                    ,("penny","853-2492")
                    ]
        
    -- We can create our own findKey function to work on this
    putStrLn( show (findKey "penny" phoneBook ))
    putStrLn( show (findKey "wilma" phoneBook ))

    -- By importing Data.Map we get some built-in functions
    -- Create map from list
    let dict = M.fromList [("betty","555-2938")
                        ,("bonnie","452-2928")
                        ,("patsy","493-2928")
                        ,("lucille","205-2928")
                        ,("wendy","939-8282")
                        ,("penny","853-2492")
                        ]
                        
    putStrLn( show ( dict ))
    
    -- Insert into map 
    putStrLn( show ( M.insert 3 100 M.empty ))

    -- Check for null (empty) map
    putStrLn( show ( M.null M.empty ))
    putStrLn( show ( M.null $ M.fromList [(2,3), (5,5)] ))

    -- Check size of map
    putStrLn( show ( M.size M.empty ))
    putStrLn( show ( M.size $ M.fromList [(2,3), (5,5)] ))
    
    -- Create a map with exactly one mapping
    putStrLn( show ( M.singleton 3 9 ))

    -- Check if key is member in map 
    putStrLn( show ( M.member 2 $ M.fromList [(2,3), (5,5)] ))

    -- Convert map to list
    putStrLn( show ( M.toList . M.insert 9 2 $ M.singleton 4 3 ))



