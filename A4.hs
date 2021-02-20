module A4 where

--Student ID: 30033104
--Student Name: Luyi Wang

-- Q1
--purpose of the function: representation of the compilePoly
--The formal parameter: it is the formula itself.
--Precoditions or assumptions: the input must is the int.
--The return value: The return value is integer representation of the formula.

data Poly = PConst Int | PVar | PAdd Poly Poly | PMul Poly Poly

compilePoly :: Poly -> (Int -> Int)
compilePoly (PConst x) = (\x -> x)
compilePoly (PVar) = id
compilePoly (PAdd a1 a2) = (\x ->(compilePoly a1 x) + (compilePoly a2 x))
compilePoly (PMul a1 a2) = (\x ->(compilePoly a1 x) * (compilePoly a2 x))

-- Q4
--purpose of the function: sum of the number that from the list but start with 0.
--The formal parameter: it is integer.
--Precoditions or assumptions: the input must is a list of int.
--The return value: The return value must a list of integer.

runningSums :: [Int] -> [Int]
runningSums iList = out
                where
                 out = 0 : zipWith (+) iList out
