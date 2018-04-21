{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : ExprDiff
Description : Provides class definition for type Expr(representing differentible equations) 
              and instances of the class. Must define functions to evaluation, simplification, 
              and partial differentiation for each instance. Instances for Expr for Doubles
              and Floats are provided.
Copyright   : (c) Connor Hewick @2018
License     : WTFPL
Maintainer  : hewicc1@mcmaster.ca
Stability   : experimental
Portability : POSIX
-}
module ExprDiff where

import ExprType

import qualified Data.Map.Strict as Map


-- * Class Definition

{- Class DiffExpr:
 -    Differentiable Expressions
 - ------------------------------------------------------------------------
 - This class has methods over the Expr datatype that
 - assist with construction and evaluation of 
 - differentiable expressions
 - ------------------------------------------------------------------------
 - Methods:
 -    eval : takes a dictionary of variable identifiers
 -        and values, and uses it to compute the Expr fully
 -    simplify : takes a possibly incomplete dictionary and uses
 -          it to reduce Expr as much as possible
 -        eg. e1 = x + y
 -          e2 = y + x
 -          simplify e1 == simplify e2
 -        eg. Add (Add (Var "x") (Const 1)) (Add (Const 2) (Var "y")) 
 -          => Add (Const 3) (Add (Var "x") (Var "y"))
 -    partDiff : given an var identifier, differentiate IN TERMS of that
 -          identifier
 - Default Methods:
 -    !+,!*,var,val,sin,cosine,ln,e : are function wrappers for Expr constructors that 
 -         perform additional simplification
 -}
-- | Class representing differentiable expressions using "ExprType"
class DiffExpr a where
  {- Methods -}
  {- eval -}
  -- | Evaluates real value of expression
  eval :: Map.Map String a -- ^ List of tuples containing variable-value pairs
                 -> Expr a -- ^ Expression of type a
                 -> a      -- ^ Value of expression as type a
  {- simplify -}               
  -- | Partially simplifies expressions into smaller equalvalent expressions
  simplify :: Map.Map String a -- ^ List of tuples containing variable-value pairs
                     -> Expr a -- ^ Expression of type a
                     -> Expr a -- ^ Simplified expression
  {- partDiff -}
  -- | Performs partial differentiation of an expression
  partDiff :: String -- ^ Variable to differentiate with respect to
           -> Expr a -- ^ Expression of type a
           -> Expr a -- ^ Resulting derivative
  {- Default Methods -}
  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2
  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2
  val :: a -> Expr a
  val x    = Const x
  var :: String -> Expr a
  var x    = Var x
  sine :: Expr a -> Expr a
  sine e   = simplify (Map.fromList []) $ Sin e
  cosine :: Expr a -> Expr a
  cosine e = simplify (Map.fromList []) $ Cos e
  ln :: Expr a -> Expr a
  ln e     = simplify (Map.fromList []) $ Ln e 
  e :: Expr a -> Expr a
  e e1     = simplify (Map.fromList []) $ E e1


{- Instance of Expr as Double:
 --------------------------------------------------------------------------
 - provides at least one case for each method eval,simplify,and partDiff 
 - for at all type constructors
 --------------------------------------------------------------------------
 - eval:
 -    links each type constructor or 'operation' to their default Haskell
 -    function, also throws an error if a varible is not defined
 - simplify:
 -    performs simple simplification on expressions
 -    Addition:
 -        turns adding identical expressions into multiplication
 -    Multplication:
 -        simplifies zero multplication and identity multiplication
 -    Constants/Variables:
 -        no simplification done
 -    Sin/Cos:
 -        simplifies sin and cos at 0 into appropriate values
 -    e/ln:
 -        shortcuts e^0 and ln(1) to 0 and 1
 -        also cancels e^ln(x) to x and ln(e^x) to x
 - partDiff:
 -    perfroms differentiation with respect to the given variable
 -    d/dx x+x = d/dx x + d/dx x
 -    d/dx x*x = ((d/dx x) * x) + (x * (d/dx x))
 -    d/dx constant = 0
 -    d/dx x = 1
 -    d/dx y = y
 -    d/dx sin(x) = cos(x) * d/dx x
 -    d/dx cos(x) = -sin(x) * d/dx x
 -    d/dx ln(x) = Error, because of lack of defintion of division
 -    d/dx e^x = e^x * d/dx x
 -}
-- | Instance definition of class DiffExpr with type Float
instance DiffExpr Float where
  {- eval -}
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Const x)    = x
  eval vrs (Var x)      = case Map.lookup x vrs of
                                Just v  -> v
                                Nothing -> error "failed lookup in eval"
  eval vrs (Sin e)      = sin (eval vrs e)
  eval vrs (Cos e)      = cos (eval vrs e)
  eval vrs (Ln e)       = log (eval vrs e)
  eval vrs (E e)        = exp (eval vrs e)
  {- simplify -}
  simplify vrs (Add (Mult (Const a) e1) (Mult (Const b) e2)) = if e1 == e2 then Mult (Const (a+b)) e1 else Add (simplify vrs e1) (simplify vrs e2)
  simplify vrs (Add (Mult e1 (Const a)) (Mult (Const b) e2)) = if e1 == e2 then Mult (Const (a+b)) e1 else Add (simplify vrs e1) (simplify vrs e2)
  simplify vrs (Add (Mult (Const a) e1) (Mult e2 (Const b))) = if e1 == e2 then Mult (Const (a+b)) e1 else Add (simplify vrs e1) (simplify vrs e2)
  simplify vrs (Add (Mult e1 (Const a)) (Mult e2 (Const b))) = if e1 == e2 then Mult (Const (a+b)) e1 else Add (simplify vrs e1) (simplify vrs e2)
  simplify vrs (Add (Mult (Const c) e1) e2) = if e1 == e2 then Mult (Const (c+1)) e1 else Add (simplify vrs e1) (simplify vrs e2)
  simplify vrs (Add e1 (Mult (Const c) e2)) = if e1 == e2 then Mult (Const (c+1)) e1 else Add (simplify vrs e1) (simplify vrs e2)
  simplify vrs (Add e1 e2)                 = if e1 == e2 then Mult (Const 2) (e1) else Add (simplify vrs e1) (simplify vrs e2)
  simplify vrs (Mult (Const 0) e)          = Const 0
  simplify vrs (Mult (Const 1) e)          = simplify vrs e
  simplify vrs (Mult e (Const 0))          = Const 0
  simplify vrs (Mult e (Const 1))          = simplify vrs e  
  simplify vrs (Const c)                   = Const c
  simplify vrs (Var x)                     = Var x
  simplify vrs (Sin (Const 0))             = Const 0
  simplify vrs (Cos (Const 0))             = Const 1
  simplify vrs (Ln (Const 1))              = Const 0
  simplify vrs (Ln (E x))                  = simplify vrs x
  simplify vrs (E (Const 0))               = Const 1
  simplify vrs (E (Ln x))                  = simplify vrs x
  {- partDiff -}
  partDiff s (Add e1 e2)  = Add (partDiff s e1) (partDiff s e2)
  partDiff s (Mult e1 e2) = Add (Mult (partDiff s e1) e2) (Mult e1 (partDiff s e2))
  partDiff s (Const _)    = Const 0
  partDiff s (Var x)      = if x == s then Const 1 else Var x
  partDiff s (Sin e)      = Mult (Cos e) (partDiff  s e)
  partDiff s (Cos e)      = Mult (Const (-1)) (Mult (Sin e) (partDiff s e))
  partDiff s (Ln e)       = error "failed partial differentiation with Ln x"
  partDiff s (E e1)       = Mult (E e1) (partDiff s e1)

-- | Instance definition of class DiffExpr with type Double
instance DiffExpr Double where
  {- eval -}
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Const x)    = x
  eval vrs (Var x)      = case Map.lookup x vrs of
                                Just v  -> v
                                Nothing -> error "failed lookup in eval"
  eval vrs (Sin e)      = sin (eval vrs e)
  eval vrs (Cos e)      = cos (eval vrs e)
  eval vrs (Ln e)       = log (eval vrs e)
  eval vrs (E e)        = exp (eval vrs e)
  {- simplify -}
  simplify vrs (Add (Mult (Const a) e1) (Mult (Const b) e2)) = if e1 == e2 then Mult (Const (a+b)) e1 else Add (simplify vrs e1) (simplify vrs e2)
  simplify vrs (Add (Mult e1 (Const a)) (Mult (Const b) e2)) = if e1 == e2 then Mult (Const (a+b)) e1 else Add (simplify vrs e1) (simplify vrs e2)
  simplify vrs (Add (Mult (Const a) e1) (Mult e2 (Const b))) = if e1 == e2 then Mult (Const (a+b)) e1 else Add (simplify vrs e1) (simplify vrs e2)
  simplify vrs (Add (Mult e1 (Const a)) (Mult e2 (Const b))) = if e1 == e2 then Mult (Const (a+b)) e1 else Add (simplify vrs e1) (simplify vrs e2)
  simplify vrs (Add (Mult (Const c) e1) e2) = if e1 == e2 then Mult (Const (c+1)) e1 else Add (simplify vrs e1) (simplify vrs e2)
  simplify vrs (Add e1 (Mult (Const c) e2)) = if e1 == e2 then Mult (Const (c+1)) e1 else Add (simplify vrs e1) (simplify vrs e2)
  simplify vrs (Add e1 e2)                 = if e1 == e2 then Mult (Const 2) (e1) else Add (simplify vrs e1) (simplify vrs e2)
  simplify vrs (Mult (Const 0) e)          = Const 0
  simplify vrs (Mult (Const 1) e)          = simplify vrs e
  simplify vrs (Mult e (Const 0))          = Const 0
  simplify vrs (Mult e (Const 1))          = simplify vrs e  
  simplify vrs (Const c)                   = Const c
  simplify vrs (Var x)                     = Var x
  simplify vrs (Sin (Const 0))             = Const 0
  simplify vrs (Cos (Const 0))             = Const 1
  simplify vrs (Ln (Const 1))              = Const 0
  simplify vrs (Ln (E x))                  = simplify vrs x
  simplify vrs (E (Const 0))               = Const 1
  simplify vrs (E (Ln x))                  = simplify vrs x
  {- partDiff -}
  partDiff s (Add e1 e2)  = Add (partDiff s e1) (partDiff s e2)
  partDiff s (Mult e1 e2) = Add (Mult (partDiff s e1) e2) (Mult e1 (partDiff s e2))
  partDiff s (Const _)    = Const 0
  partDiff s (Var x)      = if x == s then Const 1 else Var x
  partDiff s (Sin e)      = Mult (Cos e) (partDiff  s e)
  partDiff s (Cos e)      = Mult (Const (-1)) (Mult (Sin e) (partDiff s e))
  partDiff s (Ln e)       = error "failed partial differentiation with Ln x"
  partDiff s (E e1)       = Mult (E e1) (partDiff s e1)