{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- BBAE AST and Type Definitions

data TBBAE where
  TNum :: TBBAE
  TBool :: TBBAE
  deriving (Show,Eq)

data BBAE where
  Num :: Int -> BBAE
  Plus :: BBAE -> BBAE -> BBAE
  Minus :: BBAE -> BBAE -> BBAE
  Bind :: String -> BBAE -> BBAE -> BBAE
  Id :: String -> BBAE
  Boolean :: Bool -> BBAE
  And :: BBAE -> BBAE -> BBAE
  Leq :: BBAE -> BBAE -> BBAE
  IsZero :: BBAE -> BBAE
  If :: BBAE -> BBAE -> BBAE -> BBAE
  deriving (Show,Eq)

type Env = [(String,BBAE)]

type Cont = [(String,TBBAE)]

subst::String->BBAE->BBAE->(Maybe BBAE)
subst i val (Id id) = if (i == id) then return val else return (Id id)
subst i val (Plus lhs rhs) = do { l <- (subst i val lhs);
                                  r <- (subst i val rhs); 
                                  return (Plus l r) }  
subst i val (Minus lhs rhs) = do { l <- (subst i val lhs); 
                                   r <- (subst i val rhs); 
                                   return (Minus l r) } 
subst i val (And lhs rhs) = do { l <- (subst i val lhs); 
                                 r <- (subst i val rhs);
                                 return (And l r) }
subst i val (Leq lhs rhs) = do { l <- (subst i val lhs); 
                                 r <- (subst i val rhs); 
                                 return (Leq l r) }
subst i val (IsZero term) = do { t <- (subst i val term); 
                                 return (IsZero t) }
subst i val (If cond then_branch else_branch) = do { c <- (subst i val cond); 
                                                     t <- (subst i val then_branch);
                                                     e <- (subst i val else_branch); 
                                                     return (If c t e) } 
subst i val (Bind id binding_instance body) = do { b_i <- (subst i val binding_instance);
                                                   subst id b_i body }
subst _ _ n = return n 

evalS :: BBAE -> (Maybe BBAE)
evalS (Num n) = return (Num n)
evalS (Boolean b) = return (Boolean b)
evalS (Id i) = Nothing
evalS (Plus lhs rhs) = do { (Num l) <- evalS(lhs); 
                            (Num r) <- evalS(rhs); 
                            return (Num (l+r))}
evalS (Minus lhs rhs) = do { (Num l) <- evalS(lhs); 
                             (Num r) <- evalS(rhs); 
                             if r > l then Nothing else
                                 return (Num (l-r)) }
evalS (And lhs rhs) = do { (Boolean l) <- evalS(lhs); 
                           (Boolean r) <- evalS(rhs);
                           return (Boolean (l&&r)) }
evalS (Leq lhs rhs) = do { (Num l) <- evalS(lhs); 
                           (Num r) <- evalS(rhs); 
                           return (Boolean (l<=r)) }
evalS (IsZero term) = do { (Num t) <- evalS(term);
                           return (Boolean (t == 0)) }
evalS (If c t e) = do { (Boolean cond) <- evalS(c); 
                        if cond then evalS(t) else evalS(e) }
evalS (Bind id binding_instance body ) = do { b_i <- evalS(binding_instance); 
                                              subst_body <- subst id b_i body;
                                              evalS(subst_body) }

evalM :: Env -> BBAE -> (Maybe BBAE)
evalM _ (Num n) = return (Num n)
evalM _ (Boolean b) = return (Boolean b)
evalM e (Id i) = lookup i e
evalM e (Plus lhs rhs) = do { (Num l) <- (evalM e lhs);
                              (Num r) <- (evalM e rhs); 
                              return (Num (l+r)) }
evalM e (Minus lhs rhs) = do { (Num l) <- (evalM e lhs); 
                               (Num r) <- (evalM e rhs);
                               if (r > l) then Nothing else return (Num (l-r)) }
evalM e (And lhs rhs) = do { (Boolean l) <- (evalM e lhs); 
                             (Boolean r) <- (evalM e rhs); 
                             return (Boolean (l&&r)) }
evalM e (Leq lhs rhs) = do { (Num l) <- (evalM e lhs); 
                             (Num r) <- (evalM e rhs); 
                             return (Boolean (l<=r)) }
evalM e (IsZero term) = do { (Num t) <- (evalM e term);
                             return (Boolean (t == 0)) }
evalM e (If cond then_branch else_branch) = do { (Boolean c) <- (evalM e cond);
                                                 if c then (evalM e then_branch) else (evalM e else_branch) }
evalM e (Bind id binding_instance body) = do { b_i <- (evalM e binding_instance);
                                               (evalM ((id, b_i):e) body) }

testBBAE :: BBAE -> Bool
testBBAE expression = ((evalS expression) == (evalM [] expression))


typeofM :: Cont -> BBAE -> (Maybe TBBAE)
typeofM _ (Num _) = return TNum
typeofM _ (Boolean _) = return TBool
typeofM c (Id i) = lookup i c
typeofM c (Plus lhs rhs) = do { TNum <- (typeofM c lhs);
                                TNum <- (typeofM c rhs); 
                                
                                return TNum }
typeofM c (Minus lhs rhs) = do { TNum <- (typeofM c lhs); 
                                 TNum <- (typeofM c rhs); 
                                 
                                 return TNum }
typeofM c (And lhs rhs) = do { TBool <- (typeofM c lhs); 
                               TBool <- (typeofM c rhs);
                               
                               return TBool }
typeofM c (Leq lhs rhs) = do { TNum <- (typeofM c lhs);
                               TNum <- (typeofM c rhs); 
                               
                               return TBool }
typeofM c (IsZero term) = do { TNum <- (typeofM c term); 
                              return TBool }
typeofM c (If cond then_branch else_branch) = do { TBool <- (typeofM c cond);
                                                   t <- (typeofM c then_branch);
                                                   e <- (typeofM c else_branch);
                                                   
                                                   if (t == e) then return t else Nothing }
typeofM c (Bind id binding_instance body) = do { b_i <- (typeofM c binding_instance); 
                                                 (typeofM ((id, b_i):c) body) }


evalT :: BBAE -> (Maybe BBAE)
evalT expression = do { type_check <- (typeofM [] expression);
                        (evalM [] expression) }

main:: IO()
main = return ()
