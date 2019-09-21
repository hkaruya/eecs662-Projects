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
evalM (Num n) = Just n
evalM (Boolean b) = Just b
evalM (Plus l r) = do { l_prime <- evalM(l);
                        r_prime <- evalM(r); 
                        return (l_prime + r_prime) }
evalM (Minus l r) = do { l_prime <- evalM(l); 
                         r_prime <- evalM(r); 
                         if (r_prime > l_prime) then Nothing
                           else return (l_prime-r_prime) }
evalM (Mult l r) = do { l_prime <- evalM(l);
                        r_prime <- evalM(r);
                        let potential_eval = l_prime*r_prime in
                         if(0 > potential_eval) then Nothing else return potential_eval; }
evalM (Div l r) = do{ l_prime <- evalM(l);
                      r_prime <- evalM(r); 
                      if 0 == r_prime then Nothing else
                         let potential_eval = (div l_prime r_prime) in 
                            if (0 > potential_eval) then Nothing else return potential eval; }
evalM (And l r) = do { l_prime <- evalM(l);
                       r_prime <- evalM(r); 
                       return (l_prime && r_prime) }
evalM (Leq l r) = do { l_prime <- evalM(l); 
                       r_prime <- evalM(r); 
                       return (l_prime <= r_prime) }
evalM (IsZero t) = do { term <- evalM(t); 
                        return (0 == term) }
evalM (If c t e) = do { condition <- evalM(c);
                        if c then evalM(t) else evalM(c) }


evalErr :: ABE -> (Maybe ABE)
evalErr _ = Nothing -- Replace this with your interpreter

-- Type Derivation Function

typeofM :: ABE -> Maybe TABE
typeofM (Num n) = TNum
typeofM (Boolean b) = TBool
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
evalTypeM (Num n) = Just n
evalTypeM (Boolean b) = Just b
evalTypeM (Plus l r) = do { TNum <- typeofM(l); 
                            TNum <- typeofM(r); 
                            
                            l_prime <- evalTypeM(l); 
                            r_prime <- evalTypeM(r);
                            return (l_prime + r_prime) }
evalTypeM (Minus l r) = do { TNum <- typeofM(l); 
                             TNum <- typeofM(r); 
                            
                             l_prime <- evalTypeM(l); 
                             r_prime <- evalTypeM(r);
                             if r_prime > l_prime then Nothing
                                 else return (l_prime - r_prime) }
evalTypeM (Mult l r) = do { TNum <- typeofM(l);
                            TNum <- typeofM(r); 
                          
                            l_prime <- evalTypeM(l); 
                            r_prime <- evalTypeM(r); 
                            let potential_eval = l_prime*r_prime in
                             if 0 > potential_eval then Nothing 
                                else return (potential_eval) }

evalTypeM (Div l r) = do { TNum <- typeofM(l);
                           TNum <- typeofM(r); 
                           
                           l_prime <- evalTypeM(l); 
                           r_prime <- evalTypeM(l); 
                           if 0 == r_prime then Nothing else
                               let potential_eval = (div l_prime r_prine) in 
                                if 0 > potential_eval then Nothing 
                                    else return (potential_eval) }
evalTypeM (And l r) = do { TBool <- typeofM(l); 
                           TBool <- typeofM(r); 
                           
                           l_prime <- evalTypeM(l); 
                           r_prime <- evalTypeM(r); 
                           return (l_prime && r_prime) }
evalTypeM (Leq l r) = do { TNum <- typeofM(l); 
                           TNum <- typeofM(r); 
                           
                           l_prime <- evalTypeM(l); 
                           r_prime <- evalTypeM(r); 
                           return (l_prime <= r_prime); }
evalTypeM (IsZero t) = do { TNum <- typeofM(t);
                             
                            term <- evalTypeM(t); 
                            return (0 == term) }
evalTypeM (If c t e) = do { TBool <- typeofM(c);

                            condition <- evalTypeM(c);

                            then_type <- typeofM(t); 
                            else_type <- typeofM(e); 
                            
                            if (then_type /= else_type) then Nothing else 
                                if condition then evalTypeM(t) else evalTypeM(e) }
evalTypeM _ = Nothing

-- Optimizer

optimize :: ABE -> ABE
optimize (Plus l (Num 0)) = l
optimize (Plus (Num 0) r) = r
optimize (Minus l (Num 0)) = l 
optimize (Mult l (Num 1)) = l
optimize (Div l (Num 1)) = l
optimize (IsZero (Num 0)) = (Boolean True)
optimize (If (Boolean True) t _) = t
optimize (If (Boolean False) _ e) = e
optimize catch_all = catch_all

interpOptM :: ABE -> Maybe ABE
interpOptM _ = Nothing
