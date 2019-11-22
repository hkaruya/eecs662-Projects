{-# LANGUAGE GADTs #-}

-- Imports for Monads

import Control.Monad

-- FAE AST and Type Definitions

data FAE where
  Num :: Int -> FAE
  Plus :: FAE -> FAE -> FAE
  Minus :: FAE -> FAE -> FAE
  Lambda :: String -> FAE -> FAE
  App :: FAE -> FAE -> FAE
  Id :: String -> FAE
  deriving (Show,Eq)

type Env = [(String,FAE)]

evalDynFAE :: Env -> FAE -> (Maybe FAE)
evalDynFAE _ (Num n) = return (Num n)
evalDynFAE _ (Lambda i b) = return (Lambda i b)
evalDynFAE e (Id i) = lookup i e
evalDynFAE e (Plus lhs rhs) = do { (Num l) <- (evalDynFAE e lhs);
                                   (Num r) <- (evalDynFAE e rhs);
                               
                                   return (Num (l + r)) }
evalDynFAE e (Minus lhs rhs) = do { (Num l) <- (evalDynFAE e lhs);
                                    (Num r) <- (evalDynFAE e rhs);
                                    
                                    return (Num (l - r)) }
evalDynFAE e (App f a) = do { (Lambda i b) <- (evalDynFAE e f);
                              (val) <- (evalDynFAE e a); 
                              evalDynFAE ((i, val):e) b }
evalDynFAE _ _ = Nothing

data FAEValue where
  NumV :: Int -> FAEValue
  ClosureV :: String -> FAE -> Env' -> FAEValue
  deriving (Show,Eq)
  
type Env' = [(String,FAEValue)]

evalStatFAE :: Env' -> FAE -> (Maybe FAEValue)
evalStatFAE _ (Num n) = return (NumV n)
evalStatFAE e (Id i) = lookup i e
evalStatFAE e (Lambda i b) = return (ClosureV i b e)
evalStatFAE e (Plus lhs rhs) = do { (NumV l) <- (evalStatFAE e lhs); 
                                    (NumV r) <- (evalStatFAE e rhs); 
                                    
                                    return (NumV (l+r)) }
evalStatFAE e (Minus lhs rhs) = do { (NumV l) <- (evalStatFAE e lhs);
                                     (NumV r) <- (evalStatFAE e rhs); 
                                     
                                     return (NumV (l-r)) }
evalStatFAE e (App f a) = do { (ClosureV i b c) <- (evalStatFAE e f);
                               (val) <- (evalStatFAE e a); 
                               
                               evalStatFAE ((i,val):c) b}
evalStatFAE _ _ = Nothing


-- FBAE AST and Type Definitions

data FBAE where
  NumD :: Int -> FBAE
  PlusD :: FBAE -> FBAE -> FBAE
  MinusD :: FBAE -> FBAE -> FBAE
  LambdaD :: String -> FBAE -> FBAE
  AppD :: FBAE -> FBAE -> FBAE
  BindD :: String -> FBAE -> FBAE -> FBAE
  IdD :: String -> FBAE
  deriving (Show,Eq)

elabFBAE :: FBAE -> FAE
elabFBAE (NumD n) = (Num n)
elabFBAE (IdD i) = (Id i)
elabFBAE (LambdaD i b) = (Lambda i (elabFBAE b))
elabFBAE (PlusD lhs rhs) = (Plus (elabFBAE lhs) (elabFBAE rhs))
elabFBAE (MinusD lhs rhs) = (Minus (elabFBAE lhs) (elabFBAE rhs))
elabFBAE (AppD f a) = (App (elabFBAE f) (elabFBAE a))
elabFBAE (BindD i a b) = (App (Lambda i (elabFBAE b)) (elabFBAE a))


evalFBAE :: Env' -> FBAE -> (Maybe FAEValue)
evalFBAE e expression = (evalStatFAE e (elabFBAE expression))

-- FBAEC AST and Type Definitions

data FBAEC where
  NumE :: Int -> FBAEC
  PlusE :: FBAEC -> FBAEC -> FBAEC
  MinusE :: FBAEC -> FBAEC -> FBAEC
  TrueE :: FBAEC
  FalseE :: FBAEC
  AndE :: FBAEC -> FBAEC -> FBAEC
  OrE :: FBAEC -> FBAEC -> FBAEC
  NotE :: FBAEC -> FBAEC
  IfE :: FBAEC -> FBAEC -> FBAEC -> FBAEC
  LambdaE :: String -> FBAEC -> FBAEC
  AppE :: FBAEC -> FBAEC -> FBAEC
  BindE :: String -> FBAEC -> FBAEC -> FBAEC
  IdE :: String -> FBAEC
  deriving (Show,Eq)

elabFBAEC :: FBAEC -> FAE
elabFBAEC (NumE n) = (Num n)
elabFBAEC (IdE i) = (Id i)
elabFBAEC (LambdaE i b) = (Lambda i (elabFBAEC(b)))
elabFBAEC TrueE = (Lambda "m" (Lambda "n" (Id "m")))
elabFBAEC FalseE = (Lambda "m" (Lambda "n" (Id "n")))
elabFBAEC (BindE i a b) = (App (Lambda i (elabFBAEC b)) (elabFBAEC a))
elabFBAEC (AppE f a) = (App (elabFBAEC f) (elabFBAEC a))
elabFBAEC (IfE c t e) = (App (App (elabFBAEC c) (elabFBAEC t)) (elabFBAEC e)) 
elabFBAEC (AndE lhs rhs) = (App (App (elabFBAEC lhs) (elabFBAEC rhs)) (elabFBAEC FalseE))
elabFBAEC (OrE lhs rhs) = (App (App (elabFBAEC lhs) (elabFBAEC TrueE)) (elabFBAEC rhs))
elabFBAEC (NotE term) = (App (App (elabFBAEC term) (elabFBAEC FalseE)) (elabFBAEC TrueE) )
elabFBAEC _ = (Num (-1))

evalFBAEC :: Env' -> FBAEC -> Maybe FAEValue
evalFBAEC e expression = (evalStatFAE e (elabFBAEC expression))

main:: IO()
main = return ()
