-- Student ID: 30033104
-- Student Name: Luyi Wang

module Main where
import Data.Char

--purpose of the function: elem of calucation symbols.
--The formal parameter: it is the formula itself.
--Precoditions or assumptions: the input must is the char.
--The return value: The return value is Bool representation of the formula.
isOp :: char -> Bool
isOp x = elem x "+-*/%"

--purpose of the function: the meaning of those symbols.
--The formal parameter: it is the formula itself.
--Precoditions or assumptions: the input must is the char.
--The return value: The return value is Ops representation of the formula.
charToOp :: char -> Ops
charToOp x = case x of 
            '+' -> Add
            '-' -> Sub
            '*' -> Mul
            '/' -> Div
            '%' -> Mod

--purpose of the function: make a expression.
--The formal parameter: it is the formula itself.
--Precoditions or assumptions: the input must is the char and expression.
--The return value: The return value is expression representation of the formula.
makeExpr :: (Char, (Expr, (Char, (Expr, Char)))) -> Expr
makeExpr ('(', (e1,(c,(e2,')')))) = Op (charToOp c ) e1 e2

neList :: Parse a b -> Parse a [b]
neList x = build (sqn x (list x ))
                 (uncurry (:))


optional :: Parse a b -> Parse a [b]
optional a = (succeed [])
             `alt`
             (p `build` (:[]))

            
--purpose of the function: Change string to expressions.
--The formal parameter: it is the formula itself.
--Precoditions or assumptions: the input must is a list of char.
--The return value: The return value is expression representation of the formula.

StringToExpr :: [Char] -> Expr
StringToExpr ('~':xs) = Lit (0-(help1 xs))
StringToExpr (xs) = Lit (help1 xs)


--purpose of the function: helper functions that change char to lntegers.
--The formal parameter: it is the formula itself.
--Precoditions or assumptions: the input must is a list of char or char.
--The return value: The return value is Integers representation of the formula.

help1 :: [Char] -> Int
help1 [] = 0
help1 (x:xs) = (help2 x) * (10^(length xs)) + (help1 xs)

help2 :: Char -> Int
help2 x = case x of
          '0' -> 0
          '1' -> 1
          '2' -> 2
          '3' -> 3
          '4' -> 4
          '5' -> 5
          '6' -> 6
          '7' -> 7
          '8' -> 8
          '9' -> 9
