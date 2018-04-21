{-|
Module      : ExprTest
Description : Contains various expressions testing the functionality of the Expr type. 
License     : WTFPL
Maintainer  : hewicc1@mcmaster.ca
Stability   : experimental
Portability : POSIX
-}
module ExprTest where

import ExprDiff
import ExprParser
import ExprPretty
import ExprType

import qualified Data.Map.Strict as Map
import Test.QuickCheck

-- * eval Tests
-- | Tests adding to variables
evalProp1 :: Double -> Double -> Bool
evalProp1 a b = eval (Map.fromList [("x",a),("y",-b)]) (Add (Var "x") (Var "y")) == a+(-b)
evalTest1 = quickCheck evalProp1
-- | Tests chaining multiple binary operations
evalProp2 :: Double -> Double -> Bool
evalProp2 a b = eval (Map.fromList [("x",a),("y",b)]) (Mult (Var "x") (Add (Const 5) (Var "y"))) == a*(5+b)
evalTest2 = quickCheck evalProp2
-- | Tests chaining multiple unary operations
evalProp3 :: Double -> Double -> Bool
evalProp3 a b = eval (Map.fromList [("x",a),("y",b)]) (Cos (E (Var "x"))) == cos(exp(a))
evalTest3 = quickCheck evalProp3
-- * simplify Tests
-- | Testing the identity value of multiplication
simpProp1 :: Double -> Double -> Bool
simpProp1 a b = simplify (Map.fromList [("x",a),("y",b)]) (Mult (Const 1) (Var "x")) == Const a
simpTest1 = quickCheck simpProp1
-- | Testing the cancellation of e^ln()
simpProp2 :: Double -> Double -> Bool
simpProp2 a b = simplify (Map.fromList [("x",a),("y",b)]) (E (Ln (Const 1))) == Const 1
simpTest2 = quickCheck simpProp2
-- | Testing that ln(1) is equalvalent to 0
simpProp3 :: Double -> Double -> Bool
simpProp3 a b = simplify (Map.fromList [("x",a),("y",b)]) (Ln (Const 1)) == Const 0
simpTest3 = quickCheck simpProp3
-- * partDiff Tests
{-
-- | Tests if differentiating with respect to correct variable
diffProp1 ::  String -> Bool
diffProp1 s = partDiff "t" (Var s) == Const 0
-- | Tests the derivative of a constant multiplied by a variable
diffProp2 :: String -> Double -> Bool
diffProp2 s a = partDiff "s" (Mult (Const a) (Var s)) == Add (Mult (Const 0) (Var "s")) (Mult (Const a) (Const 1))
-- | Tests derivative of sin is cos
diffProp3 :: String -> Double -> Bool
diffProp3 s a = partDiff "s" (Sin (Var s)) == Mult (Cos (Var "s")) (Const 1)
-}