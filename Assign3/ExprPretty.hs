{-|
Module      : ExprPretty
Description : Provides an instance of Show for the Expr datatype to
			  be printed to the console
Copyright   : (c) Connor Hewick @2018
License     : WTFPL
Maintainer  : hewicc1@mcmaster.ca
Stability   : experimental
Portability : POSIX
-}
module ExprPretty where

import ExprType

{- Instance Show Expr 
 -      Provides a pretty representation of our datatype
 -      Matching the DSL provided in DiffExpr
 -}
-- | Provides a Show instance of Expr to be conviently displayed
instance Show a => Show (Expr a) where
  show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)
  show (Add e1 e2)  = parens (show e1) ++ " !+ " ++ parens (show e2)
  show (Var ss)     = parens $ "var \"" ++ ss ++ "\""
  show (Const x)    = parens $ "" ++ show x
  show (Sin e)      = "sin" ++ parens (show e) 
  show (Cos e)      = "cos" ++ parens (show e)
  show (Ln e)       = "ln" ++ parens (show e)
  show (E e)        = "e^" ++ parens (show e)

-- * Miscellaneous Functions
-- | Wraps a string in parentheses
parens :: String -> String
parens ss = "(" ++ ss ++ ")"