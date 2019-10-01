{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- AST Definition

data TABE where
  TNum :: TABE
  TBool :: TABE
  deriving (Show,Eq)

data ABE where
  Num :: Int -> ABE
  Plus :: ABE -> ABE -> ABE
  Minus :: ABE -> ABE -> ABE
  Mult :: ABE -> ABE -> ABE
  Div :: ABE -> ABE -> ABE
  Boolean :: Bool -> ABE
  And :: ABE -> ABE -> ABE
  Leq :: ABE -> ABE -> ABE
  IsZero :: ABE -> ABE
  If :: ABE -> ABE -> ABE -> ABE
  deriving (Show,Eq)

-- Evaluation Functions

evalM :: ABE -> (Maybe ABE)
evalM (Num n) = Just (Num n)
evalM (Boolean b) = Just (Boolean b) 
evalM (Plus l r) = do { Num l_prime <- evalM(l);
                        Num r_prime <- evalM(r); 
                        return (Num(l_prime + r_prime)) }
evalM (Minus l r) = do { Num l_prime <- evalM(l); 
                         Num r_prime <- evalM(r); 
                         if (r_prime > l_prime) then Nothing
                           else return (Num (l_prime-r_prime)) }
evalM (Mult l r) = do { Num l_prime <- evalM(l);
                        Num r_prime <- evalM(r);
                        let potential_eval = l_prime*r_prime in
                         if(0 > potential_eval) then Nothing else return (Num (potential_eval)); }
evalM (Div l r) = do{ Num l_prime <- evalM(l);
                      Num r_prime <- evalM(r); 
                      if 0 == r_prime then Nothing else
                         let potential_eval = (div l_prime r_prime) in 
                            if (0 > potential_eval) then Nothing else return (Num (potential_eval)); }
evalM (And l r) = do { Boolean l_prime <- evalM(l);
                       Boolean r_prime <- evalM(r); 
                       return (Boolean (l_prime && r_prime)) }
evalM (Leq l r) = do { Num l_prime <- evalM(l); 
                       Num r_prime <- evalM(r); 
                       return (Boolean (l_prime <= r_prime)) }
evalM (IsZero t) = do { Num term <- evalM(t); 
                        return (Boolean (0 == term)) }
evalM (If c t e) = do { Boolean condition <- evalM(c);
                        if condition then evalM(t) else evalM(c) }


evalErr :: ABE -> (Maybe ABE)
evalErr expression  = evalM(expression) -- Replace this with your interpreter

-- Type Derivation Function

typeofM :: ABE -> Maybe TABE
typeofM (Num n) = Just TNum
typeofM (Boolean b) = Just TBool
typeofM (Plus l r) = do { TNum <- typeofM(l);
                          TNum <- typeofM(r); 
                          return TNum }
typeofM (Minus l r) = do { TNum <- typeofM(l); 
                           TNum <- typeofM(r);
                           return TNum }
typeofM (Mult l r) = do { TNum <- typeofM(l); 
                          TNum <- typeofM(r); 
                          return TNum }
typeofM (Div l r) = do { TNum <- typeofM(l); 
                         TNum <- typeofM(r);
                         return TNum }
typeofM (And l r) = do { TBool <- typeofM(l); 
                         TBool <- typeofM(r); 
                         return TBool }
typeofM (Leq l r) = do { TNum <- typeofM(l); 
                         TNum <- typeofM(r); 
                         return TBool }
typeofM (IsZero t) = do { TNum <- typeofM(t);
                          return TBool }
typeofM (If c t e) = do { TBool <- typeofM(c);
                          t_branch <- typeofM(t); 
                          e_branch <- typeofM(e); 
                          if (t_branch == e_branch) then return t_branch 
                              else Nothing }

-- Combined interpreter

evalTypeM :: ABE -> Maybe ABE
evalTypeM (Num n) = Just (Num n)
evalTypeM (Boolean b) = Just (Boolean b)
evalTypeM (Plus l r) = do { TNum <- typeofM(l); 
                            TNum <- typeofM(r); 
                            
                            Num l_prime <- evalTypeM(l); 
                            Num r_prime <- evalTypeM(r);
                            return (Num (l_prime + r_prime)) }
evalTypeM (Minus l r) = do { TNum <- typeofM(l); 
                             TNum <- typeofM(r); 
                            
                             Num l_prime <- evalTypeM(l); 
                             Num r_prime <- evalTypeM(r);
                             if r_prime > l_prime then Nothing
                                 else return (Num (l_prime - r_prime)) }
evalTypeM (Mult l r) = do { TNum <- typeofM(l);
                            TNum <- typeofM(r); 
                          
                            Num l_prime <- evalTypeM(l); 
                            Num r_prime <- evalTypeM(r); 
                            let potential_eval = l_prime*r_prime in
                             if 0 > potential_eval then Nothing 
                                else return (Num (potential_eval)) }

evalTypeM (Div l r) = do { TNum <- typeofM(l);
                           TNum <- typeofM(r); 
                           
                           Num l_prime <- evalTypeM(l); 
                           Num r_prime <- evalTypeM(l); 
                           if 0 == r_prime then Nothing else
                               let potential_eval = (div l_prime r_prime) in 
                                if 0 > potential_eval then Nothing 
                                    else return (Num (potential_eval)) }
evalTypeM (And l r) = do { TBool <- typeofM(l); 
                           TBool <- typeofM(r); 
                           
                           Boolean l_prime <- evalTypeM(l); 
                           Boolean r_prime <- evalTypeM(r); 
                           return (Boolean (l_prime && r_prime)) }
evalTypeM (Leq l r) = do { TNum <- typeofM(l); 
                           TNum <- typeofM(r); 
                           
                           Num l_prime <- evalTypeM(l); 
                           Num r_prime <- evalTypeM(r); 
                           return (Boolean (l_prime <= r_prime)); }
evalTypeM (IsZero t) = do { TNum <- typeofM(t);
                             
                            Num term <- evalTypeM(t); 
                            return (Boolean (0 == term)) }
evalTypeM (If c t e) = do { TBool <- typeofM(c);

                            Boolean condition <- evalTypeM(c);

                            then_type <- typeofM(t); 
                            else_type <- typeofM(e); 
                            
                            if (then_type /= else_type) then Nothing else 
                                if condition then evalTypeM(t) else evalTypeM(e) }

-- Optimizer

optimize :: ABE -> ABE
optimize (IsZero (Num 0)) = (Boolean True)
optimize (Plus l (Num 0)) = optimize(l)
optimize (Plus (Num 0) r) = optimize(r)
optimize (Minus l (Num 0)) = optimize(l) 
optimize (Mult l (Num 1)) = optimize(l)
optimize (Mult (Num 1) r) = optimize(r)
optimize (Div l (Num 1)) = optimize(l)
optimize (If (Boolean True) t _) = optimize(t)
optimize (If (Boolean False) _ e) = optimize(e)
optimize (Plus l r) = (Plus (optimize(l)) (optimize(r)))
optimize (Minus l r) = (Minus (optimize(l)) (optimize(r)))
optimize (Mult l r) = (Mult (optimize(l)) (optimize(r)))
optimize (Div l r) = (Div (optimize(l)) (optimize(r)))
optimize (And l r) = (And (optimize(l)) (optimize(r)))
optimize (Leq l r) = (Leq (optimize(l)) (optimize(r)))
optimize (IsZero t) = (IsZero (optimize(t)))
optimize (If c t e) = (If (optimize(c)) (optimize(t)) (optimize(e)))
optimize catch_all = catch_all

interpOptM :: ABE -> Maybe ABE
interpOptM expression = evalTypeM(optimize(expression))
