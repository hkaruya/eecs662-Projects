{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

--
-- Simple caculator over naturals with no identifiers
--
-- Author: Perry Alexander
-- Date: Tue Jan 23 17:54:44 CST 2018
--
-- Source files for the Arithmetic Expressions (AE) language from PLIH
--

-- AST Definition

data AE where
  Num :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  Mult :: AE -> AE -> AE
  Div :: AE -> AE -> AE
  If0 :: AE -> AE -> AE -> AE
  deriving (Show,Eq)

-- AE Parser (Requires ParserUtils and Parsec included above)

languageDef =
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedNames = [ "if0"
                              , "then"
                              , "else"
                              ]
            , reservedOpNames = [ "+","-","*","/"]
            }
  
lexer = makeTokenParser languageDef

inFix o c a = (Infix (reservedOp lexer o >> return c) a)
preFix o c = (Prefix (reservedOp lexer o >> return c))
postFix o c = (Postfix (reservedOp lexer o >> return c))

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

expr :: Parser AE
expr = buildExpressionParser operators term

operators = [
  [ inFix "*" Mult AssocLeft
    , inFix "/" Div AssocLeft ]
  , [ inFix "+" Plus AssocLeft
  , inFix "-" Minus AssocLeft ]
  ]
  
numExpr :: Parser AE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

ifExpr :: Parser AE
ifExpr  = do reserved lexer "if0"
             c <- expr
             reserved lexer "then"
             t <- expr
             reserved lexer "else"
             e <- expr
             return (If0 c t e)
                     

term = parens lexer expr
       <|> numExpr
       <|> ifExpr

-- Parser invocation
-- Call parseAE to parse a string into the AE data structure.

parseAE = parseString expr

-- Evaluation Functions
-- Replace the bodies of these functions with your implementations for
-- Exercises 1-4.  Feel free to add utility functions or testing functions as
-- you see fit, but do not change the function signatures.  Note that only
-- Exercise 4 requires you to integrate the parser above.

evalAE :: AE -> Int
evalAE (Num n) = n
evalAE (Plus l r) = evalAE(l) + evalAE(r)
evalAE (Minus l r) = if l_prime >= r_prime then l_prime - r_prime
                                         else error "Negative number not defined in language !!!"
                     where l_prime = evalAE(l)
                           r_prime = evalAE(r)

evalAEMaybe :: AE -> Maybe Int
evalAEMaybe (Num n) = Just n
evalAEMaybe (Plus l r) = if ((Nothing == l_prime) || (Nothing == r_prime)) then Nothing
                                                                     else Just(lifted_l + lifted_r)
                   where l_prime = evalAEMaybe(l)
                         r_prime = evalAEMaybe(r)
                         Just lifted_l = l_prime
                         Just lifted_r = r_prime
evalAEMaybe (Minus l r) = if ((Nothing == l_prime) || (Nothing == r_prime)) then Nothing
                             else if lifted_l >= lifted_r then Just (lifted_l - lifted_r) else Nothing
                          where l_prime = evalAEMaybe(l)
                                r_prime = evalAEMaybe(r)
                                Just lifted_l = l_prime
                                Just lifted_r = r_prime

evalM :: AE -> Maybe Int
evalM (Num n) = Just n
evalM (Plus l r) = do { l_prime <- evalM(l);
                        r_prime <- evalM(r);
                        Just (l_prime + r_prime) }
evalM (Minus l r) = do { l_prime <- evalM(l);
                         r_prime <- evalM(r);
                         if l_prime >= r_prime then Just (l_prime - r_prime) 
                                               else Nothing }

interpAE :: String -> Maybe Int
interpAE input_string = evalM (parseAE input_string)
