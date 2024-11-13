-- Working with Lists and Tuples in Haskell
-- Example program that creates a basic Haskell program that demonstrates:
-- 1. Creating Lists
-- 2. Working with Lists
-- 3. Creating List of Range
-- 4. Working with Tuples

main :: IO ()
main = do
    -- PART 1 ****************************************
    -- Create lists using square brackets
    let numbers = [1, 2, 3, 4, 5, 6, 7]
    putStrLn(show(numbers))
    
    -- Concatenate two lists just like strings
    let moreNums = numbers ++ [8, 9, 10]
    putStrLn(show(moreNums))

    -- Can use a : to add a single element to front of list
    let evenMore = 0 : moreNums
    putStrLn(show(evenMore))
    
    -- PART 2 ****************************************
    -- We can get a specific indexed value using !!
    -- Index must be a valid index otherwise error occurs
    putStrLn(show(evenMore !! 5)) -- Get 5th index element
    
    -- Can also have lists of list like normal
    let stats = [[0, 10], [1, 31], [2, 41]]
    -- Can concatenate lists of same dimensions
    let moreStats = stats ++ [[3, 56]]
    putStrLn(show(moreStats))
    
    -- Can compare lists too
    -- First element compared, then if equal move to next
    putStrLn(show( [3, 2, 1] > [2, 1, 0] ))
    putStrLn(show( [3, 2, 1] > [2, 1, 100] ))
    putStrLn(show( [1, 200, 1000] > [2, 1, 100] ))

    -- Lists in Haskell operate like Prolog lists
    -- both have a head and tail
    let a = [1, 2, 3, 4, 5]
    putStrLn("First element: " ++ show(head a))
    putStrLn("Other elements: " ++ show(tail a))
    putStrLn("Last element: " ++ show(last a))
    putStrLn("All but last element: " ++ show(init a))

    -- We can have other methods to use on lists as well
    let b = []
    putStrLn("A Length: " ++ show(length a))
    putStrLn("B Empty: " ++ show(null b))
    putStrLn("A Reverse: " ++ show(reverse a))
    putStrLn("First two of A: " ++ show(take 2 a))
    putStrLn("Without first two: " ++ show(drop 2 a))
    putStrLn("Smallest in A: " ++ show(minimum a))
    putStrLn("Largest in A: " ++ show(maximum a))
    putStrLn("Sum of A: " ++ show(sum a))
    putStrLn("Product of A: " ++ show(product a))
    
    -- We can also check to see if a value is in a list 
    putStrLn("5 in A? " ++ show(5 `elem` a))

    -- PART 3 ****************************************
    -- We can create a list of a range of values 
    let l = [1..20]
    putStrLn("L: " ++ show(l))
    let chars = ['a'..'z']
    putStrLn("Chars: " ++ show(chars))
    
    -- We can also specify a pattern to follow
    -- Haskell looks at pattern and continues it
    let lBy2 = [2,4..20]
    putStrLn("L By 2: " ++ show(lBy2))
    let lBy3 = [2,5..20]
    putStrLn("L By 3: " ++ show(lBy3))
    let chars2 = ['a','c'..'z']
    putStrLn("Chars2: " ++ show(chars2))
    
    -- To go opposite way just specify pattern downwards
    let lTo0 = [20,19..0]
    putStrLn("Reverse L: " ++ show(lTo0))
    
    -- Be careful when using floating point numbers
    -- results may vary
    let fl = [0.1, 0.3 .. 1]
    putStrLn("Floats: " ++ show(fl))
    
    -- We can also make infinite range lists 
    -- due to Haskells lazy nature
    let mul2 = [2,4..]
    -- Be careful when printing without take as infinite loop
    putStrLn("Multiples of 2: " ++ show(take 10 mul2))
    
    -- Can also use cycle and repeat too
    -- Be aware these are infinite lists as well
    let _123 = cycle [1,2,3]
    putStrLn("1 2 3 Cycle: " ++ show(take 8 _123))
    let zeros = repeat 0
    putStrLn("All O's: " ++ show(take 8 zeros))

    -- We can also use set notation to create lists too
    -- We can apply a filtering predicate x*2 >= 12
    let set = [x*2 | x <- [1..10], x*2 >= 12]
    putStrLn("Set Result: " ++ show(set))
    -- We can apply as many filters as we want too
    let set2 = [x | x <- [1..50], x /= 13, x /= 15, x /= 37]
    putStrLn("Set2 Result: " ++ show(set2))
    
    -- We can draw from multiple lists too
    let combinations = [x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]
    putStrLn("Combinations: " ++ show(combinations))

    -- We can do the same thing with strings
    let nouns = ["hobo","frog","pope"]
    let adjectives = ["lazy","grouchy","scheming"]
    let wordCombos = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]
    putStrLn("Word Combos: " ++ show(wordCombos))

    -- We can create our own length function
    let length' x = sum [1 | _ <- x]
    putStrLn("Word Combos Length: " ++ show(length' wordCombos))

    -- We can also edit strings in same way since Strings are lists
    let removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']] 
    let text = "HaHaHaHaHa"
    putStrLn(text ++ " No Lower: " ++ show(removeNonUppercase text))
    
    -- PART 4 ****************************************
    -- We can make tuples using ()
    -- Tuples do not require same data type 
    -- Each different sized tuple is its own data type
    -- You cannot write a general function for all tuple sizes
    let vec1 = [(1,2), (2, 30), (3, 40)]
    let vec2 = [("John",21), ("Sue", 18), ("Ali", 25)]
    -- We can use fst and snd to pull apart two part tuples
    -- Note these do not work on 3-tuples, 4-tuples, ...
    putStrLn("Name: " ++ show( fst (vec2 !! 0) ))
    putStrLn("Age: " ++ show( snd (vec2 !! 0) ))
    
    -- We can use zip to pull two lists together into pairs
    -- If list lengths don't match longer list truncated to
    -- length of shorter list
    let z = zip ["Jim", "John", "Jill"] [21, 22, 23]
    putStrLn("People: " ++ show(z))

    
    