-- HC5T1: Apply a function three times to an integer
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (f (f x))

-- HC5T2: Filter odd numbers from list 1 to 30
filterOdds :: [Int]
filterOdds = filter odd [1..30]

-- HC5T3: Check if any word starts with uppercase letter
import Data.Char (isUpper)
anyStartsWithUpper :: [String] -> Bool
anyStartsWithUpper = any (\word -> not (null word) && isUpper (head word))

-- HC5T4: Rewrite biggerThan10 using a lambda function
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

-- HC5T5: Partial application to multiply by 5
multiplyByFive :: Int -> Int
multiplyByFive = (*5)

-- HC5T6: Function composition: squares then filter even numbers
squareEven :: [Int] -> [Int]
squareEven = filter even . map (^2)

-- HC5T7: Rewrite using $ operator
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

-- HC5T8: Point-free style version of addFive
addFive :: Int -> Int
addFive = (+5)

-- HC5T9: Higher-order function to apply a function twice to every element of a list
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

-- HC5T10: Combine filter, map, any to check if any squared value > 50
anySquareGreater50 :: [Int] -> Bool
anySquareGreater50 = any (>50) . map (^2) . filter (const True)  -- filter kept as placeholder (could be more complex)

-- Main to demonstrate all tasks
main :: IO ()
main = do
  putStrLn "=== HC5T1: Apply Function Thrice ==="
  print $ applyThrice (+1) 0
  print $ applyThrice (*2) 1

  putStrLn "\n=== HC5T2: Filter Odd Numbers 1-30 ==="
  print filterOdds

  putStrLn "\n=== HC5T3: Any Word Starts with Uppercase? ==="
  print $ anyStartsWithUpper ["hello", "world"]
  print $ anyStartsWithUpper ["Hello", "world"]

  putStrLn "\n=== HC5T4: biggerThan10 Lambda ==="
  print $ biggerThan10 5
  print $ biggerThan10 15

  putStrLn "\n=== HC5T5: Multiply By Five ==="
  print $ multiplyByFive 3
  print $ multiplyByFive 10

  putStrLn "\n=== HC5T6: Square then Filter Even ==="
  print $ squareEven [1..10]

  putStrLn "\n=== HC5T7: Result Using $ Operator ==="
  print result

  putStrLn "\n=== HC5T8: addFive Point-Free ==="
  print $ addFive 10

  putStrLn "\n=== HC5T9: transformList Applies Function Twice ==="
  print $ transformList (+3) [1, 2, 3]

  putStrLn "\n=== HC5T10: Any Squared Value > 50 ==="
  print $ anySquareGreater50 [1..7]
  print $ anySquareGreater50 [1..5]

