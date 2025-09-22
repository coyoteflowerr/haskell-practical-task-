-- HC4T1 - Weather Report using pattern matching
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"

-- HC4T2 - Day Type using pattern matching
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday"   = "It's a weekend!"
dayType "Monday"   = "It's a weekday."
dayType "Tuesday"  = "It's a weekday."
dayType "Wednesday"= "It's a weekday."
dayType "Thursday" = "It's a weekday."
dayType "Friday"   = "It's a weekday."
dayType _          = "Invalid day"

-- HC4T3 - Grade Comment using guards
gradeComment :: Int -> String
gradeComment grade
  | grade >= 90 && grade <= 100 = "Excellent!"
  | grade >= 70 && grade <= 89  = "Good job!"
  | grade >= 50 && grade <= 69  = "You passed."
  | grade >= 0  && grade <= 49  = "Better luck next time."
  | otherwise                   = "Invalid grade"

-- HC4T4 - Special Birthday with pattern matching
specialBirthday :: Int -> String
specialBirthday 1  = "First birthday! So cute!"
specialBirthday 18 = "Adult at last!"
specialBirthday 50 = "Half a century old!"
-- HC4T5 - Catch-all pattern with age in message
specialBirthday age = "Happy " ++ show age ++ "th birthday!"

-- HC4T6 - Identify list contents
whatsInsideThisList :: [a] -> String
whatsInsideThisList []        = "The list is empty."
whatsInsideThisList [x]       = "The list has one item."
whatsInsideThisList [x, y]    = "The list has two items."
whatsInsideThisList _         = "The list has many items."

-- HC4T7 - First and Third elements only
firstAndThird :: [a] -> Maybe (a, a)
firstAndThird (x:_:z:_) = Just (x, z)
firstAndThird _         = Nothing

-- HC4T8 - Describe a tuple
describeTuple :: (Int, String) -> String
describeTuple (n, s) = "The number is " ++ show n ++ " and the string is \"" ++ s ++ "\"."

-- MAIN FUNCTION
main :: IO ()
main = do
  putStrLn "-- HC4T1: weatherReport --"
  print (weatherReport "sunny")
  print (weatherReport "cloudy")
  print (weatherReport "stormy")

  putStrLn "\n-- HC4T2: dayType --"
  print (dayType "Saturday")
  print (dayType "Monday")
  print (dayType "Funday")

  putStrLn "\n-- HC4T3: gradeComment --"
  print (gradeComment 95)
  print (gradeComment 75)
  print (gradeComment 55)
  print (gradeComment 30)
  print (gradeComment 105)

  putStrLn "\n-- HC4T4 & HC4T5: specialBirthday --"
  print (specialBirthday 1)
  print (specialBirthday 18)
  print (specialBirthday 50)
  print (specialBirthday 33)

  putStrLn "\n-- HC4T6: whatsInsideThisList --"
  print (whatsInsideThisList ([] :: [Int]))
  print (whatsInsideThisList [1])
  print (whatsInsideThisList [1, 2])
  print (whatsInsideThisList [1, 2, 3])

  putStrLn "\n-- HC4T7: firstAndThird --"
  print (firstAndThird [10, 20, 30])    -- Just (10, 30)
  print (firstAndThird [1, 2])          -- Nothing

  putStrLn "\n-- HC4T8: describeTuple --"
  print (describeTuple (42, "Answer"))
