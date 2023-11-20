{-#LANGUAGE GADTs#-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Datas where

------------------------
{- Tipos -}
------------------------

type Nom = String
type Mem = [(Nom,Int)]

data Exp where
    V :: Nom -> Exp
    I :: Int -> Exp
    (:+) :: Exp -> Exp -> Exp
    (:-) :: Exp -> Exp -> Exp
    (:*) :: Exp -> Exp -> Exp
  deriving Eq

data BExp where 
    T :: BExp
    F :: BExp
    (:&&) :: BExp -> BExp -> BExp
    (:||) :: BExp -> BExp -> BExp
    NOT :: BExp ->BExp
    (:==) :: Exp -> Exp -> BExp
  deriving Eq

data Prog where
    ASIG :: [(Nom, Exp)] ->Prog
    (:>) :: Prog ->Prog ->Prog
    IF:: BExp ->Prog ->Prog ->Prog
    WHILE:: BExp ->Prog ->Prog
  deriving Eq
 
------------------------
{- Precedencias -}
------------------------ 
 
-- :* tiene mayor precedencia que :+ y :-
infixl 8 :*
infixl 6 :+
infixl 6 :-
-- :&& tiene mayor precedencia que :||
infixl 4 :&&
infixl 3 :||
-- :== tiene la mayor precedencia
infixl 5 :==
-- :> tiene la menor precedencia
infixr 3 :>


------------------------
{- Instancias de Show -}
------------------------
instance Show Exp where
    show = toString

instance Show BExp where
    show = toString

instance Show Prog where
    show = indentPs 0
    
-----------------------------------------
{- Definición e instancias de ToString -}
-----------------------------------------
class ToString a where
    toString :: a -> String

instance ToString Prog where
    toString (ASIG xs)   = if null l then "{}" else l 
        where 
            l = '[' : (concat $ mapInit (++", ") (map (\(x,y) -> x ++ "=" ++ toString y) xs)) ++ "]"
            mapInit f (x:xs@(y:ys)) = f x:mapInit f xs
            mapInit f l = l
    toString (WHILE bexp _)  = "WHILE (" ++ toString bexp ++ ") {\n"
    toString (IF bexp _ _)  = "IF (" ++ toString bexp ++ ") {\n"

instance ToString Exp where
    toString (V x) = x
    toString (I x) = show x
    toString e
        | isAtomic e1 = toString e1 ++ op ++ if isAtomic e2 then toString e2 else ("(" ++ toString e2 ++ ")")
        | isAtomic e2 = "("++ toString e1 ++ ")" ++ op ++ toString e2
        | otherwise   = "("++ toString e1 ++ ")" ++ op ++ "(" ++ toString e2 ++ ")"
      where (e1,e2,op) = getParts e
            getParts (e1 :+ e2) = (e1,e2,":+")
            getParts (e1 :* e2) = (e1,e2,":*")
            getParts (e1 :- e2) = (e1,e2,":-")

instance ToString BExp where
    toString T = "True"
    toString F = "False"
    toString (NOT e) = "NOT("++ toString e ++")"
    toString (e1 :== e2)
        | isAtomic e1 = toString e1 ++ " == " ++ if isAtomic e2 then toString e2 else ("(" ++ toString e2 ++ ")")
        | isAtomic e2 = "("++ toString e1 ++ ")" ++ " == " ++ toString e2
        | otherwise   = "("++ toString e1 ++ ")" ++ " == " ++ "(" ++ toString e2 ++ ")"
    toString e
        | isAtomicB e1 = toString e1 ++ op ++ if isAtomicB e2 then toString e2 else ("(" ++ toString e2 ++ ")")
        | isAtomicB e2 = "("++ toString e1 ++ ")" ++ op ++ toString e2
        | otherwise   = "("++ toString e1 ++ ")" ++ op ++ "(" ++ toString e2 ++ ")"
      where (e1,e2,op) = getParts e
            isAtomicB T = True
            isAtomicB F = True
            isAtomicB _ = False
            getParts (e1 :&& e2) = (e1,e2,":&&")
            getParts (e1 :|| e2) = (e1,e2,":||")
 
---------------------------
{- Funciones auxiliares  -}
---------------------------
isAtomic :: Exp -> Bool
isAtomic (V _) = True
isAtomic (I _) = True
isAtomic _ = False

indentPs :: Int -> Prog -> String
indentPs n (p1 :> p2)      = indentPs n p1 ++ ";\n" ++ indentPs n p2
indentPs n p@(ASIG xs)     = indent n $ toString p
indentPs n p@(IF b p1 p2)  = indent n $ toString p ++ format (n+1) p1 ++ indent n "} ELSE {\n"++ format (n+1) p2 ++ indent n "}"
indentPs n p@(WHILE b p2)  = indent n $ toString p ++ format (n+1) p2 ++ indent n "}"

format :: Int -> Prog -> String
format n p = indentPs (n+1) p ++ "\n"

indent :: Int -> String -> String
indent n s = concat (take n (repeat "  ")) ++ s
