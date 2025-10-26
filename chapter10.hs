-- Haskell Chapter 10 Practical Tasks: Custom Type Classes and Instances

-- ======================================================
-- HC10T1: ShowSimple Type Class
-- ======================================================

data PaymentMethod = Cash | Card | Cryptocurrency deriving (Show, Eq)

class ShowSimple a where
  showSimple :: a -> String

instance ShowSimple PaymentMethod where
  showSimple Cash = "Cash Payment"
  showSimple Card = "Card Payment"
  showSimple Cryptocurrency = "Crypto Payment"


-- ======================================================
-- HC10T2: Summable Type Class
-- ======================================================

class Summable a where
  sumUp :: [a] -> a

instance Summable Int where
  sumUp = sum


-- ======================================================
-- HC10T3: Comparable Type Class
-- ======================================================

data Blockchain = Bitcoin | Ethereum | Solana deriving (Show, Eq)

class Comparable a where
  compareWith :: a -> a -> Ordering

instance Comparable Blockchain where
  compareWith Bitcoin Bitcoin = EQ
  compareWith Ethereum Ethereum = EQ
  compareWith Solana Solana = EQ
  compareWith Bitcoin _ = LT
  compareWith Ethereum Bitcoin = GT
  compareWith Ethereum _ = LT
  compareWith Solana _ = GT


-- ======================================================
-- HC10T4: Eq Instance for Box
-- ======================================================

data Box a = Empty | Has a deriving (Show)

instance Eq a => Eq (Box a) where
  Empty == Empty = True
  Has x == Has y = x == y
  _ == _ = False


-- ======================================================
-- HC10T5: ShowDetailed Type Class
-- ======================================================

data User = User
  { username :: String
  , age :: Int
  } deriving (Show)

class ShowDetailed a where
  showDetailed :: a -> String

instance ShowDetailed User where
  showDetailed (User name age) =
    "User: " ++ name ++ ", Age: " ++ show age


-- ======================================================
-- HC10T6: Mutual Recursion in Eq for Blockchain
-- ======================================================

instance Eq Blockchain where
  (==) a b = compareWith a b == EQ
  (/=) a b = not (a == b)


-- ======================================================
-- HC10T7: Convertible Type Class
-- ======================================================

class Convertible a b where
  convert :: a -> b

instance Convertible PaymentMethod String where
  convert Cash = "Cash"
  convert Card = "Card"
  convert Cryptocurrency = "Cryptocurrency"


-- ======================================================
-- HC10T8: AdvancedEq Subclass of Eq
-- ======================================================

class Eq a => AdvancedEq a where
  compareEquality :: a -> a -> Bool
  compareEquality x y = x == y


instance AdvancedEq Int


-- ======================================================
-- HC10T9: MinMax Type Class
-- ======================================================

class MinMax a where
  minValue :: a
  maxValue :: a

instance MinMax Int where
  minValue = minBound
  maxValue = maxBound


-- ======================================================
-- HC10T10: Concatenatable Type Class
-- ======================================================

class Concatenatable a where
  concatWith :: a -> a -> a

instance Concatenatable [Char] where
  concatWith = (++)


-- ======================================================
-- MAIN: Demonstrate All Tasks
-- ======================================================

main :: IO ()
main = do
  putStrLn "=== HC10T1: ShowSimple ==="
  putStrLn $ showSimple Cash
  putStrLn $ showSimple Card
  putStrLn $ showSimple Cryptocurrency

  putStrLn "\n=== HC10T2: Summable ==="
  print (sumUp [1,2,3,4,5 :: Int])

  putStrLn "\n=== HC10T3: Comparable ==="
  print (compareWith Bitcoin Ethereum)
  print (compareWith Solana Bitcoin)

  putStrLn "\n=== HC10T4: Eq for Box ==="
  print (Has 10 == Has 10)
  print (Has 5 == Has 6)
  print (Empty == (Empty :: Box Int))

  putStrLn "\n=== HC10T5: ShowDetailed ==="
  let u = User "Alice" 30
  putStrLn (showDetailed u)

  putStrLn "\n=== HC10T6: Mutual Recursion Eq for Blockchain ==="
  print (Bitcoin == Bitcoin)
  print (Bitcoin /= Ethereum)

  putStrLn "\n=== HC10T7: Convertible ==="
  putStrLn (convert Card)
  putStrLn (convert Cryptocurrency)

  putStrLn "\n=== HC10T8: AdvancedEq ==="
  print (compareEquality (5 :: Int) 5)
  print (compareEquality (5 :: Int) 6)

  putStrLn "\n=== HC10T9: MinMax ==="
  putStrLn $ "Min Int: " ++ show (minValue :: Int)
  putStrLn $ "Max Int: " ++ show (maxValue :: Int)

  putStrLn "\n=== HC10T10: Concatenatable ==="
  putStrLn $ concatWith "Hello, " "World!"
