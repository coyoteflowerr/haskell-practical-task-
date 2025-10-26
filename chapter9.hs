-- Haskell Chapter 9 Practical Tasks: Parametric Types and Recursion

-- HC9T1: Define a Parametric Type Synonym
type Entity a = (String, a)
-- Example: Entity Int = ("Address", 100)


-- HC9T2: Implement a Parametric Data Type
data Box a = Empty | Has a deriving Show
-- Example: Has 5 or Empty


-- HC9T3: Function to Add Values in a Box
addN :: Num a => a -> Box a -> Box a
addN n Empty    = Empty
addN n (Has x)  = Has (x + n)


-- HC9T4: Extract a Value from a Box
extract :: a -> Box a -> a
extract def Empty   = def
extract _   (Has x) = x


-- HC9T5: Parametric Data Type with Record Syntax
data Shape a
  = Circle { color :: a, radius :: Float }
  | Rectangle { color :: a, width :: Float, height :: Float }
  deriving Show


-- HC9T6: Recursive Data Type for Tweets
data Tweet = Tweet
  { content  :: String
  , likes    :: Int
  , comments :: [Tweet]
  } deriving Show


-- HC9T7: Engagement Function for Tweets
engagement :: Tweet -> Int
engagement (Tweet _ l cs) = l + sum (map engagement cs)


-- HC9T8: Recursive Sequence Data Type
data Sequence a = End | Node a (Sequence a) deriving Show


-- HC9T9: Check for Element in a Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ End = False
elemSeq x (Node y ys)
  | x == y    = True
  | otherwise = elemSeq x ys


-- HC9T10: Binary Search Tree Data Type
data BST a = EmptyTree | NodeBST a (BST a) (BST a) deriving Show


-- ======================================================
-- Demonstration section
-- ======================================================

main :: IO ()
main = do
  putStrLn "=== HC9T1: Parametric Type Synonym ==="
  let entity1 :: Entity Int
      entity1 = ("Wallet", 500)
  print entity1

  putStrLn "\n=== HC9T2: Box Type ==="
  print (Has 10)
  print (Empty :: Box Int)

  putStrLn "\n=== HC9T3: addN ==="
  print (addN 5 (Has 10))
  print (addN 5 Empty)

  putStrLn "\n=== HC9T4: extract ==="
  print (extract 0 (Has 42))
  print (extract 0 Empty)

  putStrLn "\n=== HC9T5: Parametric Shape ==="
  let c = Circle { color = "Red", radius = 5.0 }
  let r = Rectangle { color = "Blue", width = 10, height = 4 }
  print c
  print r

  putStrLn "\n=== HC9T6 & HC9T7: Tweets and Engagement ==="
  let reply1 = Tweet "Nice!" 2 []
  let reply2 = Tweet "Awesome!" 3 []
  let mainTweet = Tweet "Check out my post!" 10 [reply1, reply2]
  print mainTweet
  putStrLn $ "Total engagement: " ++ show (engagement mainTweet)

  putStrLn "\n=== HC9T8 & HC9T9: Sequence ==="
  let seq1 = Node 1 (Node 2 (Node 3 End))
  print seq1
  putStrLn $ "Is 2 in sequence? " ++ show (elemSeq 2 seq1)
  putStrLn $ "Is 5 in sequence? " ++ show (elemSeq 5 seq1)

  putStrLn "\n=== HC9T10: Binary Search Tree ==="
  let tree = NodeBST 10
                (NodeBST 5 EmptyTree EmptyTree)
                (NodeBST 15 EmptyTree EmptyTree)
  print tree
