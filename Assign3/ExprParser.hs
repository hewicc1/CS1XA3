{-|
Module      : ExprParser
Description : Creates parsers to fully parse string inputs into
              Expr expressions of type Double and Float.
Copyright   : (c) Connor Hewick @2018
License     : WTFPL
Maintainer  : hewicc1@mcmaster.ca
Stability   : experimental
Portability : POSIX
-}
module ExprParser  where


import ExprType

import Text.Parsec
import Text.Parsec.String

-- * Float Parsers

-- | Attempts to parses a string into an expression of type Float. If invalid expression throws error.
parseExprF :: String -- ^ input string for attempted parse
       -> Expr Float -- ^ resulting expression in Expr type of Float
parseExprF ss = case parse exprF "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr
-- | Parses each operator individually
exprF :: Parser (Expr Float)
exprF = unaryOpsF <|> binaryOpsF
-- ** Unary Operations
-- | Parses all unary operators
unaryOpsF :: Parser (Expr Float)
unaryOpsF = parseVarF <|> parseConstF <|> parseSinF <|> parseCosF <|> parseLnF <|> parseEF
-- | Parses all binary operators
binaryOpsF :: Parser (Expr Float)
binaryOpsF = parseMultF <|> parseAddF
-- | Parses Variables
parseVarF :: Parser (Expr Float)
parseVarF = do { spaces;
                 symbol "/";
                 v <- many1 letter;
                 return (Var v)  
               }
-- | Parses Constants of Float
parseConstF :: Parser (Expr Float)
parseConstF = do { spaces;
                   symbol "_";
                   fl <- float;
                   return (Const fl)
                 }
-- | Parses Sin function of Float
parseSinF :: Parser (Expr Float)
parseSinF = do { spaces;
                 symbol "sin";
                 v <- between (symbol "(") (symbol ")") exprF;
                 return (Sin v)
               }
-- | Parses Cos function of Float
parseCosF :: Parser (Expr Float)
parseCosF = do { spaces;
                 symbol "cos";
                 v <- between (symbol "(") (symbol ")") exprF;
                 return (Cos v)
               }
-- | Parses natural logarithm function of Float
parseLnF :: Parser (Expr Float)
parseLnF = do { spaces;
                symbol "ln";
                v <- between (symbol "(") (symbol ")") exprF;
                return (Ln v)
              }
-- | Parses natural exponential function of Float
parseEF :: Parser (Expr Float)
parseEF = do { spaces;
                symbol "e^";
                v <- between (symbol "(") (symbol ")") exprF;
                return (E v)
              }
-- ** Binary Operations
-- | Parses binary multiplication
parseMultF :: Parser (Expr Float)
parseMultF = do { spaces;
                  symbol "mul";
                  t1 <- between (symbol "(") (symbol ")") exprF;
                  t2 <- between (symbol "(") (symbol ")") exprF;
                  return (Mult t1 t2)
                }
-- | Parses binary addition
parseAddF :: Parser (Expr Float)
parseAddF = do { spaces;
                  symbol "add";
                  t1 <- between (symbol "(") (symbol ")") exprF;
                  t2 <- between (symbol "(") (symbol ")") exprF;
                  return (Add t1 t2)
                }


-- * Double Parsers

-- | Attempts to parses a string into an expression of type Double. ID invalid expression throws error.
parseExprD :: String -- ^ input string for attempted parse
       -> Expr Double -- ^ resulting expression in Expr type of Double
parseExprD ss = case parse exprD "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr
-- | Parses each operator individually
exprD :: Parser (Expr Double)
exprD = unaryOpsD <|> binaryOpsD
-- ** Unary Operations
-- | Parses all unary operators
unaryOpsD :: Parser (Expr Double)
unaryOpsD = parseVarD <|> parseConstD <|> parseSinD <|> parseCosD <|> parseLnD <|> parseED
-- | Parses all binary operators
binaryOpsD :: Parser (Expr Double)
binaryOpsD = parseMultD <|> parseAddD
-- | Parses Variables
parseVarD :: Parser (Expr Double)
parseVarD = do { spaces;
                 symbol "/";
                 v <- many1 letter;
                 return (Var v)  
               }
-- | Parses Constants of Double
parseConstD :: Parser (Expr Double)
parseConstD = do { spaces;
                   symbol "_";
                   db <- double;
                   return (Const db)
                 }
-- | Parses Sin Dunction of Double
parseSinD :: Parser (Expr Double)
parseSinD = do { spaces;
                 symbol "sin";
                 v <- between (symbol "(") (symbol ")") exprD;
                 return (Sin v)
               }
-- | Parses Cos Dunction of Double
parseCosD :: Parser (Expr Double)
parseCosD = do { spaces;
                 symbol "cos";
                 v <- between (symbol "(") (symbol ")") exprD;
                 return (Cos v)
               }
-- | Parses natural logarithm Dunction of Double
parseLnD :: Parser (Expr Double)
parseLnD = do { spaces;
                symbol "ln";
                v <- between (symbol "(") (symbol ")") exprD;
                return (Ln v)
              }
-- | Parses natural exponential Dunction of Double
parseED :: Parser (Expr Double)
parseED = do { spaces;
                symbol "e^";
                v <- between (symbol "(") (symbol ")") exprD;
                return (E v)
              }
-- ** Binary Operations
-- | Parses binary multiplication
parseMultD :: Parser (Expr Double)
parseMultD = do { spaces;
                  symbol "mul";
                  t1 <- between (symbol "(") (symbol ")") exprD;
                  t2 <- between (symbol "(") (symbol ")") exprD;
                  return (Mult t1 t2)
                }
-- | Parses binary addition
parseAddD :: Parser (Expr Double)
parseAddD = do { spaces;
                  symbol "add";
                  t1 <- between (symbol "(") (symbol ")") exprD;
                  t2 <- between (symbol "(") (symbol ")") exprD;
                  return (Add t1 t2)
                }

-- * Miscellaneous Parsers

parens :: Parser a -> Parser a
parens p = do { char '(';
               cs <- p;
               char ')';
               return cs }

symbol :: String -> Parser String
symbol ss = let
 symbol' :: Parser String
 symbol' = do { spaces;
                ss' <- string ss;
                spaces;
                return ss' }
 in try symbol'

digits :: Parser String
digits = many1 digit

negDigits :: Parser String
negDigits = do { neg <- symbol "-";
                dig <- digits;
                return (neg ++ dig) }

integer :: Parser Integer
integer = fmap read $ try negDigits <|> digits

decimalDigits :: Parser String
decimalDigits = do { d <- char '.';
                     rm <- digits;
                     return $ d:rm }

negDecimalDigits :: Parser String
negDecimalDigits = do { ds <- try negDigits <|> digits;
                   rs <- try decimalDigits <|> return "";
                   return $ ds ++ rs }

double :: Parser Double
double = fmap read $ negDecimalDigits

float :: Parser Float
float = fmap read $ negDecimalDigits
