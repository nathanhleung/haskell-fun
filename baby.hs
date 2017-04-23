-- from http://learnyouahaskell.com/starting-out

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

-- if statement must have else
-- everything needs to return a value
doubleSmallNumber x = if x > 100
                        then x
                        else doubleMe x
                        
-- prime ' signifies variation of function
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

-- Storing a value
conanO'Brien = "It's a-me, Conan O'Brien!"

-- List of lists
b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]

c = [1,2,3,4,5,6,7]

-- Stepped range
d = [2,4..20]

-- List with predicate (set comprehension)
-- take every number from 1 to 10, if its double is
-- greater than/equal to 12, store double
e = [x * 2 | x <- [1..10], x * 2 >= 12]

-- take numbers from 50 to 100
-- if x % 7 == 3, then store the number
f = [ x | x <- [50..100], x `mod` 7 == 3]

-- put list comprehension into a function
fizzBuzz x =
    if (x `mod` 15 == 0)
        then "FizzBuzz"
        else if (x `mod` 3 == 0)
            then "Fizz"
            else if (x `mod` 5 == 0)
                then "Buzz"
                else ""

-- Run fizzBuzz on all list elements
fizzBuzzList xs = [ fizzBuzz x | x <- xs ]

products = [x*y | x <- [2,5,10], y <- [8,10,11]]


-- words
nouns = ["president", "cat", "dalai lama"]
adjectives = ["fun", "scheming", "cool"]

combined = [adjective ++ " " ++ noun | noun <- nouns, adjective <- adjectives]

-- custom length function
-- makes list with element 1 for each element in original list
length' list = sum [1 | _ <- list]

-- factorial
factorial x = product [1..x]

-- remove non uppercase
removeNonUppercase string = [ch | ch <- string, ch `elem` ['A'..'Z']]