# CS 1XA3 Assignment #3: Haskell Math Library
## Documentation:
[Here](hewicc1.github.uo)
## Goal:
Build a math library in haskell for helping you with your Calculus homework.
### Functionality:
Able to represent expressions using the following operators:
1. Addition (Add)
2. Multiplication (Mult)
3. Variables (Var)
4. Constants (Const)
5. Sine Function (Sin)
6. Cosine Function (Cos)
7. Natural Logarithm (Ln)
8. Natural Exponential (E)
### Instructions for Parsing:
Use functions parseExprF for Floats and parseExprD for Doubles in the syntax of:
        parseExprF "expression here"
To represent:
* Addition -> "add ( ) ( )" eg. add (_5) (/x)
* Multiplication -> "mul ( ) ( )" eg. mul (_6) (_2)
* Variables -> "/" eg. /x
* Constants -> "_" eg. _5
* Sine Function -> "sin( )" eg. sin(0)
* Cosine Function -> "cos( )" eg. cos(0)
* Natural Logarithm Function -> "ln( )" eg. ln(1)
* Natural Exponential Function -> "e^( )" eg. e^(0)
### Modules:
#### ExprType
Provides type definition for type Expr(representing differentible equations) and function to retrieve variables from an expression.
#### ExprDiff
Provides class definition for type Expr(representing differentible equations) and instances of the class. Must define functions to evaluation, simplification, and partial differentiation for each instance. Instances for Expr for Doubles and Floats are provided.
#### ExprPretty
Provides an instance of Show for the Expr datatype to be printed to the console
#### ExprParser
Creates parsers to fully parse string inputs into Expr expressions of type Double and Float.
#### ExprTest
Contains various expressions testing the functionality of the Expr type.
## License: WTFPL
## Citations:
* referenced fellow classmate aksamitn's work for parsing [Here](https://github.com/aksamitn/1XA3-MathIt/blob/master/ExprParser.hs)
* referenced fellow classmate barskyn's work for testing [Here](https://github.com/barskyn/CS1XA3/blob/master/Assign3/assign3/ExprTest.hs)
