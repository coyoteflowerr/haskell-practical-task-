-- HC8T1: Type Synonyms and Basic Function
type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx fromAddr toAddr val =
  "From: " ++ fromAddr ++ ", To: " ++ toAddr ++ ", Value: " ++ show val

-- HC8T2: New Types and Data Constructors
data PaymentMethod = Cash | Card | Cryptocurrency deriving Show

data Person = Person String (String, Int) PaymentMethod deriving Show

bob :: Person
bob = Person "Bob" ("123 Haskell Lane", 101) Cash

-- HC8T3: Algebraic Data Types and Functions
data Shape = Circle Float | Rectangle Float Float deriving Show

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

circleArea :: Float
circleArea = area (Circle 5)

rectangleArea :: Float
rectangleArea = area (Rectangle 10 5)

-- HC8T4: Record Syntax for Employee
data Employee = Employee { name :: String, experienceInYears :: Float } deriving Show

richard :: Employee
richard = Employee { name = "Richard", experienceInYears = 7.5 }

-- HC8T5: Record Syntax for Person
data PersonRecord = PersonRecord
  { pname :: String
  , age :: Int
  , isEmployed :: Bool
  } deriving Show

person1 :: PersonRecord
person1 = PersonRecord "Alice" 30 True

person2 :: PersonRecord
person2 = PersonRecord "Eve" 22 False

-- HC8T6: Record Syntax for Shape Variants
data ShapeVariant
  = CircleRecord { center :: (Float, Float), color :: String, radius :: Float }
  | RectangleRecord { width :: Float, height :: Float, color :: String }
  deriving Show

circleShape :: ShapeVariant
circleShape = CircleRecord { center = (0,0), color = "Red", radius = 10 }

rectangleShape :: ShapeVariant
rectangleShape = RectangleRecord { width = 4, height = 6, color = "Blue" }

-- HC8T7: Data Types and Describing Animals
data Animal = Dog String | Cat String deriving Show

describeAnimal :: Animal -> String
describeAnimal (Dog name) = "This is a dog named " ++ name
describeAnimal (Cat name) = "This is a cat named " ++ name

dog1 :: Animal
dog1 = Dog "Buddy"

cat1 :: Animal
cat1 = Cat "Whiskers"

-- HC8T8: Type Synonyms and Greeting Function
type Name = String
type Age = Int

greet :: Name -> Age -> String
greet n a = "Hello, my name is " ++ n ++ " and I am " ++ show a ++ " years old."

-- HC8T9: Record Type and Transaction Function
data Transaction = Transaction
  { from :: Address
  , to :: Address
  , amount :: Value
  , transactionId :: String
  } deriving Show

createTransaction :: Address -> Address -> Value -> String
createTransaction f t amt =
  let txId = "tx_" ++ take 5 (show (abs (hash f + hash t + amt)))
  in show (Transaction { from = f, to = t, amount = amt, transactionId = txId })

-- Simple hash function (for demonstration only)
hash :: String -> Int
hash = sum . map fromEnum

-- HC8T10: Deriving Show for Book
data Book = Book { title :: String, author :: String, year :: Int } deriving Show

book1 :: Book
book1 = Book "Learn You a Haskell" "Miran Lipovača" 2011

-- Main function
main :: IO ()
main = do
  putStrLn "--- HC8T1 ---"
  putStrLn $ generateTx "addr1" "addr2" 100

  putStrLn "\n--- HC8T2 ---"
  print bob

  putStrLn "\n--- HC8T3 ---"
  putStrLn $ "Circle Area: " ++ show circleArea
  putStrLn $ "Rectangle Area: " ++ show rectangleArea

  putStrLn "\n--- HC8T4 ---"
  print richard

  putStrLn "\n--- HC8T5 ---"
  print person1
  print person2

  putStrLn "\n--- HC8T6 ---"
  print circleShape
  print rectangleShape

  putStrLn "\n--- HC8T7 ---"
  putStrLn $ describeAnimal dog1
  putStrLn $ describeAnimal cat1

  putStrLn "\n--- HC8T8 ---"
  putStrLn $ greet "Charlie" 25

  putStrLn "\n--- HC8T9 ---"
  putStrLn $ createTransaction "addrA" "addrB" 500

  putStrLn "\n--- HC8T10 ---"
  print book1
