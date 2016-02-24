{-# OPTIONS -Wall #-}


-- | This module demonstrates how to use the Charted library.

module Demo where

import Charted

import Control.Monad (forM_)







-- | An @Expr@ represents the concrete syntax of mathematical expressions.

data Expr
  = Var String
  | Op String
  | BinApp Expr Expr Expr
  deriving (Eq)

instance Show Expr where
  show (Var x) = x
  show (Op o) = o
  show (BinApp x o y) =
    "(" ++ show x ++ " " ++ show o ++ " " ++ show y ++ ")"





-- | The syntactic types for a concrete expression. This can be either @NUM@,
-- indicating that the expression is a numeric expression, or @OP@, which is
-- the type of an operator expression.

data Type = NUM | OP
  deriving (Show,Eq,Ord)





-- | Our grammar consists of just one 
grammar :: Grammar Expr Type
grammar = [ op ]
  where
    op = do NUM <- nextLabel
            x <- nextData
            OP <- nextLabel
            p <- nextData
            NUM <- nextLabel
            y <- nextData
            return (BinApp x p y, NUM)





-- | An operator symbol is lexed as an @OP@ expression, otherwise its a @NUM@
-- type variable.

lexer :: Lexer Expr Type
lexer x
  | x `elem` ["+","-","*","/"]
    = [ (Op x, OP) ]
  | otherwise
    = [ (Var x, NUM) ]





-- | This input is highly ambiguous, as there are many possible ways to insert
-- parentheses around binary applications.

expr :: String
expr = "a + b - c * d / e"





main :: IO ()
main = do putStrLn ""
          putStrLn ("The expression to parse is: " ++ expr)
          putStrLn ("There are " ++ show (length parses) ++ " parses.")
          putStrLn "The parses are:"
          forM_ parses $ \(p,t) ->
            putStrLn ("  " ++ show p ++ "   of type   " ++ show t)
          putStrLn ""
  where
    parses = parse grammar lexer expr