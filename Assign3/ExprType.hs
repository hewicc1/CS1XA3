{-|
Module      : ExprType
Description : Provides type definition for type Expr(representing differentible equations) 
              and function to retrieve variables from an expression.
Copyright   : (c) Connor Hewick @2018
License     : WTFPL
Maintainer  : hewicc1@mcmaster.ca
Stability   : experimental
Portability : POSIX
-}
module ExprType where

import Data.List

-- * DataType Decleration

{- Expression Data Type
 - -----------------------------------------------
 - Wraps different Operations in a Expression Tree
 - Ops:
 -       Add   - standard binary addition
 -       Mult  - standard binary multiplication
 -       Const - wrapper for simple values
 -       Var   - string identifier for variables
 -      Sin   - standard sin function
 -      Cos   - standard cos function
 -      Ln    - logarithmic function with a base 
 -                  of eulers constant, e
 -      E     - Represents eulers constant, e to 
 -                  an exponent
 -}

-- | A datatype for common numeric expression
data Expr a = Add (Expr a) (Expr a)  -- ^ Binary Addition
            | Mult (Expr a) (Expr a) -- ^ Binary Multiplication
            | Const a                -- ^ Value Wrapper
            | Var String             -- ^ Variable Identifier
            | Sin (Expr a)           -- ^ Unary Sine
            | Cos (Expr a)           -- ^ Unary Cosine
            | Ln (Expr a)            -- ^ Unary Natural Logarithm
            | E (Expr a)             -- ^ Unary Natural Exponential
  deriving Eq

-- * Micellaneous Functions

{- getVars:
 -      Decomposes expressions recursively to obtain all
 -      variables in the expression
 -}

-- | A function for retrieving the variables out of an expression
getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 `union` getVars e2
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Const _)    = []
getVars (Var ident)  = [ident]
getVars (Sin e)      = getVars e
getVars (Cos e)      = getVars e
getVars (Ln e)       = getVars e
getVars (E e)        = getVars e