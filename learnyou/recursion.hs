-- http://learnyouahaskell.com/recursion

-- recursive max
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Can't get max of empty list"
maximum' [x] = x
maximum' xs = if head xs > maximum' (tail xs)
                    then head xs 
                    else maximum' (tail xs)
                    
-- another recursive max
maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "empty"
maximum'' [x] = x
maximum'' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum'' xs
    
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0     = []
    | otherwise = x:(replicate' (n - 1) x)
    
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n xs
    | n > fromIntegral (length xs) = error "not enough elements in list to take"
    | n <= 0        = []
    | otherwise     = head xs:take' (n - 1) (tail xs)
    
reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = last xs:reverse' (init xs)

reverse'' [] = []
reverse'' (x:xs) = reverse'' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [a] -> [(a, a)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = elem' a xs
    
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    quicksort [y | y <- xs, y < x] ++ -- elements less than the head
    [x] ++ -- the head
    quicksort [z | z <- xs, z >= x] -- elements greater than or equal to the head
    
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
    let smallerElements = [a | a <- xs, a < x]
        largerElements = [a | a <- xs, a >= x]
    in quicksort' smallerElements ++ [x] ++ quicksort' largerElements