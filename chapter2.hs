-- HC2T1 - Task 1: Checking Types (Expected Types Output)

-- HC2T2 - Task 2: Function Type Signatures and Implementations

add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2

-- HC2T3 - Task 3: Immutable Variables

myAge :: Int
myAge = 25

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

-- HC2T4 - Task 4: Infix and Prefix Examples

-- Infix to Prefix (Examples)
infixToPrefix1 = (+) 5 3           -- 8
infixToPrefix2 = (*) 10 4          -- 40
infixToPrefix3 = (&&) True False   -- False

-- Prefix to Infix (Examples)
prefixToInfix1 = 7 + 2             -- 9
prefixToInfix2 = 6 * 5             -- 30
prefixToInfix3 = True && False     -- False

-- HC2T5 - Task 5: Defining and Using Functions

circleArea :: Float -> Float
circleArea r = pi * r * r

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z = max x (max y z)

-- HC2T6 - Task 6: Int vs Integer

smallNumber :: Int
smallNumber = 262

bigNumber :: Integer
bigNumber = 2127

-- Uncomment to test overflow behavior
-- overflowExample = 2^64 :: Int     -- Likely overflow
-- noOverflow = 2^64 :: Integer      -- Works fine

-- HC2T7 - Task 7: Boolean Expressions

boolAndTrue :: Bool
boolAndTrue = True && True          -- True

boolOrFalse :: Bool
boolOrFalse = False || False        -- False

boolNot :: Bool
boolNot = not False                 -- True

boolComparison :: Bool
boolComparison = 5 > 10             -- False

-- Main function for testing all tasks
main :: IO ()
main = do
    -- HC2T1 Output
    putStrLn "HC2T1 - Checking Types (Expected Types)"
    putStrLn "Expression: 42"
    putStrLn "Expected type: Num a => a"

    putStrLn "Expression: 3.14"
    putStrLn "Expected type: Fractional a => a"

    putStrLn "Expression: \"Haskell\""
    putStrLn "Expected type: [Char]"

    putStrLn "Expression: 'Z'"
    putStrLn "Expected type: Char"

    putStrLn "Expression: True && False"
    putStrLn "Expected type: Bool"

    -- HC2T2 Examples
    putStrLn "\nHC2T2 - Function Examples"
    print (add 3 4)
    print (isEven 4)
    print (concatStrings "Hello, " "World!")

    -- HC2T4 Examples
    putStrLn "\nHC2T4 - Infix and Prefix Notation Examples"
    print infixToPrefix1
    print infixToPrefix2
    print infixToPrefix3
    print prefixToInfix1
    print prefixToInfix2
    print prefixToInfix3

    -- HC2T5 Function Testing
    putStrLn "\nHC2T5 - Function Testing"
    print (circleArea 3.0)         -- ~28.27
    print (maxOfThree 5 12 8)      -- 12

    -- HC2T6 - Int vs Integer
    putStrLn "\nHC2T6 - Int vs Integer"
    print smallNumber
    print bigNumber
    -- Evaluate 2^64 manually in GHCi for overflow test

    -- HC2T7 - Boolean Expressions
    putStrLn "\nHC2T7 - Boolean Expressions"
    print boolAndTrue
    print boolOrFalse
    print boolNot
    print boolComparison
