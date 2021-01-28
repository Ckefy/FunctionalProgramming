module Arithmetic where

import Control.Monad

data ArithmeticError
  = ZeroDivision
  | NegativePow
  deriving (Show, Eq)

data Expr
  = Const Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving (Show, Eq)

-- | Evaluate the result of expression
-- | It handles errors, if occurred 
eval :: Expr -> Either ArithmeticError Int
eval (Const curElem) = return curElem
eval (Add first second) = liftEval2 first (+) second
eval (Sub first second) = liftEval2 first (-) second
eval (Mul first second) = liftEval2 first (*) second
eval (Div first second) = eval second >>= throwDivision >>= liftEval2 first div
eval (Pow first second) = eval second >>= throwPow >>= liftEval2 first (^)

--binary lift
-- | Lift binary function in context
-- | And evaluate arguments
liftEval2 :: Expr -> (Int -> Int -> Int) -> Expr -> Either ArithmeticError Int
liftEval2 first func second = liftM2 func (eval first) (eval second)

--Left for handling errors, Right for answer
-- | Error handler for division error
throwDivision :: Int -> Either ArithmeticError Expr
throwDivision curElem =
   if curElem == 0
     then Left ZeroDivision
     else Right (Const curElem)

-- | Error handler for power error
throwPow :: Int -> Either ArithmeticError Expr
throwPow curElem =
   if curElem < 0
     then Left NegativePow
     else Right (Const curElem)
