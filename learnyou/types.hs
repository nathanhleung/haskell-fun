-- http://learnyouahaskell.com/types-and-typeclasses

removeUppercase :: String -> String
removeUppercase str = [ch | ch <- str, ch `elem` ['a'..'z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

sphereVolume :: Double -> Double
sphereVolume r  = 4 / 3 * pi * r^2

-- http://learnyouahaskell.com/syntax-in-functions
lucky :: (Integral a) => a -> String
lucky 7 = "You win"
lucky x = "You lose"

addVectors :: Num a => (a, a, a) -> (a, a, a) -> (a, a, a)
addVectors (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

-- triple pattern matching
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, c) = c

-- x:xs pattern test
patterns = [(x,y,xs) | x:y:xs <- [[1,2,3,5],[3,4,5,10],[1,2],[2]]]

-- head'
head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "Empty list"
tell (x:[]) = "The list has 1 element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long."

-- recursion
length' :: [a] -> Int
length' [] = 0
length' (_:rest) = length' rest + 1

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (curr:rest) = curr + sum' rest

-- pattern matching to the whole
firstLetter :: String -> String
firstLetter "" = "Empty string"
firstLetter str@(ch:_) = "The first letter of " ++ str ++ " is " ++ [ch]

bmiMessage :: Double -> String
bmiMessage bmi
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "Overweight"
    | otherwise   = "Obese"
    
bmiMessage' weight height
    | weight / height ^ 2 <= 18.5 = "Underweight"
    | weight / height ^ 2 <= 25.0 = "Normal"
    | weight / height ^ 2 <= 30.0 = "Overweight"
    | otherwise   = "Obese"
    
max' :: (Ord a) => a -> a -> a
max' x y
    | x > y     = x
    | otherwise = y
    
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a < b     = LT
    | otherwise = EQ
    
bmiMessage'' :: Double -> Double -> String
bmiMessage'' weight height
    | bmi <= underweight = "Underweight"
    | bmi <= normal      = "Normal"
    | bmi <= overweight  = "Overweight"
    | otherwise          = "Obese"
    where bmi = weight / height ^ 2
          underweight = 18.5
          normal = 25.0
          overweight = 30.0
          
initials :: String -> String -> (Char, Char)
-- pattern matching
initials (f:_) (l:_) = (f,l)

-- initials with `where` bindings
initials' :: String -> String -> String
initials' firstName lastName = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstName
          (l:_) = lastName
          
-- bmi calculator over list of widths and heights
bmiMapper :: [(Double, Double)] -> [Double]
bmiMapper list = [bmi weight height | (weight, height) <- list]
    where bmi weight height = weight / height ^ 2
    
-- same as above but turns numbers into messages
bmiMapper':: [(Double, Double)] -> [String]
bmiMapper' list = [bmiMessage bmi | bmi <- (bmiMapper list)]

-- cylinder SA with let
cylinder :: Double -> Double -> Double
cylinder radius height =
    let lateralArea = 2 * pi * radius * height
        baseArea    = pi * radius ^ 2
    in lateralArea + 2 * baseArea
    
-- cylinder SA with where
cylinder' :: Double -> Double -> Double
cylinder' radius height =
    lateralArea + 2 * baseArea
    where lateralArea = 2 * pi * radius * height
          baseArea    = pi * radius ^ 2
          
-- let binding local scope
squares = let squareMe x = x * x in (squareMe 1, squareMe 3, squareMe 5)

-- let binding list
cubes = [cubed | x <- [1..12], let cubed = x ^ 3]

-- case expression
describeList :: [a] -> String
describeList list = case list of
                        [] -> "empty"
                        [_] -> "singleton"
                        list -> "long"