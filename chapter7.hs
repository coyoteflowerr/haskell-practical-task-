-- HC7T1: Implement an Eq Instance for a Custom Data Type
data Color = Red | Green | Blue deriving (Show, Read, Bounded, Enum)

instance Eq Color where
    Red   == Red   = True
    Green == Green = True
    Blue  == Blue  = True
    _     == _     = False

-- HC7T2: Implement an Ord Instance for a Custom Data Type
instance Ord Color where
    compare Red Green   = LT
    compare Red Blue    = LT
    compare Green Blue  = LT
    compare Green Red   = GT
    compare Blue Red    = GT
    compare Blue Green  = GT
    compare _ _         = EQ

-- HC7T3: Function Using Multiple Constraints
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y = if x >= y then x else y

-- HC7T4: Custom Type with Show and Read
data Shape = Circle Double | Rectangle Double Double deriving (Eq)

instance Show Shape where
    show (Circle r) = "Circle " ++ show r
    show (Rectangle w h) = "Rectangle " ++ show w ++ " " ++ show h

instance Read Shape where
    readsPrec _ input = 
        case words input of
            ("Circle":r:_) -> [(Circle (read r), "")]
            ("Rectangle":w:h:_) -> [(Rectangle (read w) (read h), "")]
            _ -> []

-- HC7T5: Function with Num Constraint
squareArea :: Num a => a -> a
squareArea side = side * side

-- HC7T6: Using Integral and Floating Type Classes
circleCircumference :: (Real a, Floating b) => a -> b
circleCircumference r = 2 * pi * realToFrac r

-- HC7T7: Bounded and Enum
nextColor :: Color -> Color
nextColor color
    | color == maxBound = minBound
    | otherwise         = succ color

-- HC7T8: Parse a Value from a String Using Read
parseShape :: String -> Maybe Shape
parseShape str = case reads str of
    [(shape, "")] -> Just shape
    _             -> Nothing

-- HC7T9: Type Class with Multiple Instances
class Describable a where
    describe :: a -> String

instance Describable Bool where
    describe True  = "This is true."
    describe False = "This is false."

instance Describable Shape where
    describe (Circle r) = "A circle with radius " ++ show r
    describe (Rectangle w h) = "A rectangle with width " ++ show w ++ " and height " ++ show h

-- HC7T10: Function with Multiple Type Class Constraints
describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y = describe (compareValues x y)

-- Main function to demonstrate all the tasks
main :: IO ()
main = do
    -- HC7T1 & HC7T2
    putStrLn "HC7T1 & HC7T2: Comparing Colors"
    print (Red == Red)
    print (Red < Green)
    print (compareValues Red Blue)

    -- HC7T3
    putStrLn "\nHC7T3: Compare Values"
    print (compareValues 10 20)
    print (compareValues "apple" "banana")

    -- HC7T4
    putStrLn "\nHC7T4: Show and Read Shape"
    let shape1 = Circle 3.5
    let shape2 = Rectangle 4 5
    print shape1
    print shape2
    print (read "Circle 3.5" :: Shape)
    print (read "Rectangle 4.0 5.0" :: Shape)

    -- HC7T5
    putStrLn "\nHC7T5: Square Area"
    print (squareArea 4)
    print (squareArea 4.5)

    -- HC7T6
    putStrLn "\nHC7T6: Circle Circumference"
    print (circleCircumference (7 :: Int))
    print (circleCircumference (7.0 :: Double))

    -- HC7T7
    putStrLn "\nHC7T7: Next Color"
    print (nextColor Red)
    print (nextColor Green)
    print (nextColor Blue)

    -- HC7T8
    putStrLn "\nHC7T8: Parse Shape"
    print (parseShape "Circle 3.5")
    print (parseShape "Rectangle 3.0 4.0")
    print (parseShape "Triangle 3 4 5") -- Invalid

    -- HC7T9
    putStrLn "\nHC7T9: Describable Instances"
    print (describe True)
    print (describe shape2)

    -- HC7T10
    putStrLn "\nHC7T10: Describe and Compare"
    print (describeAndCompare (Rectangle 3 4) (Rectangle 5 6))
