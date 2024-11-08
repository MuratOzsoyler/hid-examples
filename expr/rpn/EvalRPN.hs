module EvalRPN where

import Control.Monad.State
import Data.Foldable (traverse_)

{-
   Function evalRPN evaluates an expression given
   in the reversed polish notation (RPN, postfix notation):

   evalRPN "2 3 +" ==> 5 (== "2 + 3")
   evalRPN "2 3 4 + *" ==> 14 (== 2 * (3 + 4))
   evalRPN "3 2 -" ==> 1 (== "3 - 2")
-}

type Stack = [Integer]

type EvalM = State Stack

push :: Integer -> EvalM ()
push x = modify (x :)

pop :: EvalM Integer
pop = do
  xs <- get
  put (tail xs)
  pure (head xs)

evalRPN :: String -> Integer
evalRPN expr = evalState evalRPN' []
  where
    evalRPN' = traverse_ step (words expr) >> pop
    step "+" = processTops (+)
    step "*" = processTops (*)
    step "-" = processTops (-)
    step t = push (read t)
    processTops op = flip op <$> pop <*> pop >>= push
