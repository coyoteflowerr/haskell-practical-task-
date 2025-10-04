-- HC6T1: Factorial (Recursive)
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- HC6T2: Fibonacci (Recursive)
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- HC6T3: Sum of Elements Using foldr
sumList :: [Int] -> Int
sumList = foldr (+) 0

-- HC6T4: Product of Elements Using foldl
productList :: [Int] -> Int
productList = foldl (*) 1

-- HC6T5: Reverse a List (Recursive)
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- HC6T6: Element Exists in List
elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists e (x:xs)
    | e == x    = True
    | otherwise = elementExists e xs

-- HC6T7: List Length
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

-- HC6T8: Filter Even Numbers
filterEvens :: [Int] -> [Int]
filterEvens = filter even

-- HC6T9: Map Implementation
mapFunction :: (a -> b) -> [a] -> [b]
mapFunction _ [] = []
mapFunction f (x:xs) = f x : mapFunction f xs

-- HC6T10: Digits of a Number (Recursive)
digits :: Int -> [Int]
digits n
    | n < 10    = [n]
    | otherwise = digits (n `div` 10) ++ [n `mod` 10]

-- Main function to demonstrate each task
main :: IO ()
main = do
    putStrLn "HC6T1: Factorial of 5"
    print (factorial 5)

    putStrLn "\nHC6T2: 10th Fibonacci number"
    print (fibonacci 10)

    putStrLn "\nHC6T3: Sum of [1,2,3,4,5] using foldr"
    print (sumList [1,2,3,4,5])

    putStrLn "\nHC6T4: Product of [1,2,3,4,5] using foldl"
    print (productList [1,2,3,4,5])

    putStrLn "\nHC6T5: Reverse [1,2,3,4,5]"
    print (reverseList [1,2,3,4,5])

    putStrLn "\nHC6T6: Does 3 exist in [1,2,3,4,5]?"
    print (elementExists 3 [1,2,3,4,5])

    putStrLn "\nHC6T7: Length of [1,2,3,4,5]"
    print (listLength [1,2,3,4,5])

    putStrLn "\nHC6T8: Filter even numbers from [1,2,3,4,5,6]"
    print (filterEvens [1,2,3,4,5,6])

    putStrLn "\nHC6T9: Map (*2) to [1,2,3,4,5]"
    print (mapFunction (*2) [1,2,3,4,5])

    putStrLn "\nHC6T10: Digits of 12345"
    print (digits 12345)
