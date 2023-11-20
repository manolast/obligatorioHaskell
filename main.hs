{-#LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-tabs #-}


type Nom = String


data Exp where {
    V :: Nom -> Exp;
    I :: Int -> Exp;
    (:+) :: Exp -> Exp -> Exp;
    (:-) :: Exp -> Exp -> Exp;
    (:*) :: Exp -> Exp -> Exp 
}

data BExp where {
    T :: BExp;
    F :: BExp;
    (:&&) :: BExp -> BExp -> BExp;
    (:||) :: BExp -> BExp -> BExp;
    NOT :: BExp ->BExp;
    (:==) :: Exp -> Exp -> BExp 
}

type Mem = [(Nom,Int)]

-- Se pide definir estas funciones en Haskell:
-- (1) (@@) :: Nom ->Mem ->Int, que implementa la operaci´on de lookup.
-- (2) upd :: (Nom, Int) ->Mem ->Mem, que implementa la operaci´on de update.
-- (3) eval :: Exp ->Mem ->Int, que implementa la operaci´on eval.
-- (4) beval :: BExp ->Mem ->Bool, que implementa la operaci´on beval.

(@@) :: Nom ->Mem ->Int
(@@) = \nombre mem -> case mem of{
    [] -> error ("(@@): La variable "++ nombre ++" no esta en la memoria");
    ((nom, valor):ps) -> case nom == nombre of{
        True -> valor;
        False -> nombre @@ ps
    }
}

upd :: (Nom, Int) ->Mem ->Mem
upd = \(s, n) m -> case m of{
    [] -> [(s, n)];
    (nom, valor):ps -> case nom == s of{
        True -> (s, n):ps;
        False -> [(nom, valor)] ++ upd (s, n) ps
    }
}

eval :: Exp ->Mem ->Int
eval = \e mem -> case e of{
    I x -> x;
    V nombre -> nombre @@ mem;
    n :+ m -> (eval n mem) + (eval m mem);
    n :- m -> (eval n mem) - (eval m mem);
    n :* m -> (eval n mem) * (eval m mem);
}

beval :: BExp ->Mem ->Bool
beval = \be mem -> case be of{
    T -> True;
    F -> False;
    m :&& n -> (beval m mem) && (beval n mem);
    m :|| n -> (beval m mem) || (beval n mem);
    NOT m -> not (beval m mem);
    (e1 :== e2) -> (eval e1 mem) == (eval e2 mem)
}