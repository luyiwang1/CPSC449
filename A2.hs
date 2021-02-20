module A1 where
import Data.Char
{-
 CPSC 449 Assignment 1
 UCID: 30033104
 Studemt name: Luyi Wang
-}

{-
	Question 1
	purpose of the function: Using list comprehension and primitive recursion to implement the function, which take every letters out and capitalized all.
	The formal parameter: There is a string contains letters(char), numbers(int) and symbols.
	Precoditions or assumptions: The input has to be a string.
	The return value: The return value is a string contains only letters also all capitalized.
-}

alphaToUpper :: String -> String
alphaToUpper [] = []
alphaToUpper a = [toUpper n | n <- a , (n >= 'a' && n <= 'z') || (n >= 'A' && n <= 'Z')]

alphaToUpperRec :: String -> String
alphaToUpperRec [] = []
alphaToUpperRec (y:ys) | isAlpha y = (toUpper y: alphaToUpperRec ys)
                       | otherwise = alphaToUpperRec ys



{-
	Question 2
	purpose of the function: Using primitive recursion to implement a function which convert the binary number into a natural number.
	The formal parameter: There is a string contains numbers(int).
	Precoditions or assumptions: The input must be String and output is Integer
	The return value: The return value is an Integer contains natural numbers.
-}

parseBin :: String -> Integer
parseBin "" = 0
parseBin i = other (toInteger(length i)) i

other :: Integer -> String -> Integer
other _ [] = 0
other n (y:ys) = (2 ^ (n-1)* toInteger(digitToInt(y))) + other (n-1) ys

{-
	Question 2
	purpose of the function: Using primitive recursion to implement a function which convert the natural number into binary numbrs.
	The formal parameter: There is a Integer contains numbers(int).
	Precoditions or assumptions: The input must be Integer and output is String.
	The return value: The return value is an Integer contains binary numbers.
-}

encodeBin :: Integer -> String
encodeBin 0 = "0"
encodeBin 1 = "1"
encodeBin n | n `mod` 2 == 1 = encodeBin (n `div` 2) ++ "1"
            | n `mod` 2 == 0 = encodeBin (n `div` 2) ++ "0"

{-
	Question 3
	purpose of the function: Using primitive recursion to implement two lists merge into one list.
	The formal parameter: There are two list of Integer contains sorted numbers.
	Precoditions or assumptions: The input must be two sorted lists.
	The return value: The return value must be a sorted list which contains both input list's Integers.
-}
mergeLists :: [Integer] -> [Integer] -> [Integer]
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists (x:xs) (y:ys)
  | x <= y    = x:mergeLists xs (y:ys)
  | otherwise = y:mergeLists (x:xs) ys

{-
	purpose of the function:Using primitive recursion to implement one list split into two lists.
	The formal parameter: There is one list of Integers contains sorted numbers.
	Precoditions or assumptions: The input must be a sorted list.
	The return value: The return value must be two sorted lists which contains odd index and even index.
-}
splitList :: [Integer] ->  ([Integer],[Integer])
splitList [] = ([],[])
splitList [x] = ([x],[])
splitList (x:y:rest) = (x:xs ,y:ys)
    where (xs,ys) = splitList rest

{-
	purpose of the function: Using primitive recursion to implement a function that performs merge sort. 
	The formal parameter: There is a list of Integer contains non-sorted numbers.
	Precoditions or assumptions: The input is a non-sorted list.
	The return value: The return value must be a sorted list which contains input list's Integers.
-}
mSort :: [Integer] -> [Integer]
mSort [] = []
mSort [x] = [x]
mSort xs = mergeLists (mSort as)(mSort bs)
    where 
      (as,bs) = splitList xs