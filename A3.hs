module A3 where
-- Q1

--(a)
-- Declare a haskell algebraic type Formula to represent wffs.
-- which a propositional variable, the negation of a wff, the conjunction of two wffs and the disjunction of two wffs. 
data Formula = Variable String | Not Formula | And Formula Formula | Or Formula Formula

-- (b)
-- a Haskell expression tjay constructs a formula representation of the formula "~(~p^~q)"
expression :: Formula -> Formula -> Formula
expression n m = Not (And (Not n) (Not m))

--(c)
--purpose of the function: representation of the formula f.
--The formal parameter: it is the formula itself.
--Precoditions or assumptions: the input must is the right form of the formula.
--The return value: The return value is a string representation of the formula f.
showFormula :: Formula -> String
showFormula (Variable string) = string
showFormula (Not formula) = "¬" ++ "(" ++ (showFormula formula) ++ ")"
showFormula (And fone ftwo) = "(" ++ (showFormula fone) ++ ")" ++ "∧" ++ "(" ++ (showFormula ftwo) ++ ")"
showFormula (Or fone ftwo) = "(" ++ (showFormula fone) ++ ")" ++ "∨" ++ "(" ++ (showFormula ftwo) ++ ")"

-- (d)
--purpose of the function: a function logically equivalent to f.
--The formal parameter: it is the formula itself.
--Precoditions or assumptions: the input must is the right form of the formula.
--The return value: The return value is logically equivalent to f.
rewrite :: Formula -> Formula
rewrite (Variable string) = Variable string
rewrite (Not formula) = Not (rewrite formula)
rewrite (And fone ftwo) = And (rewrite fone) (rewrite ftwo)
rewrite (Or fone ftwo) = Not (And (Not (rewrite fone))(Not (rewrite ftwo)))


-- Q3
--(a)
--purpose of the function: calculate the variance of list of Float.
--The formal parameter: a list of Float.
--Precoditions or assumptions: the input must be a list of Float.
--The return value: The return value is a Float which contains the variance of the list.
avg :: [Float] -> Float 
avg xs = sum xs/ fromIntegral(length xs)

variance :: [Float] -> Float
variance xs = realToFrac (sum (map (\x -> (x - (avg xs)) ** 2)xs)) / (fromIntegral ((length xs) - 1))

--(b)
count :: (Integer -> Bool) -> [Integer] -> Integer
count function n = fromIntegral (length (filter function n))



-- Q4
--purpose of the function: calculate the number poweroftwo.
--The formal parameter: Integer.
--Precoditions or assumptions: the input must be Integer.
--The return value: The return value is an intger.
other :: Integer -> (a -> a) -> a -> a
other 0 function x = x
other n function x = other (n - 1) function (function x)

powerOfTwo :: Integer -> Integer
powerOfTwo t = other t (\x -> 2*x) 1
