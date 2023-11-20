{-# OPTIONS_GHC -fno-warn-tabs #-}
{-#LANGUAGE GADTs#-}

-- Nombre: Manuel Stapff
-- Nro. estudiante: 303636
-- Nombre: Nicolás Píriz
-- Nro. estudiante: 310896

module Prog where
import Datas

-- Memoria y evaluación de expresiones
-- 1 
(@@) :: Nom -> Mem -> Int
(@@) = \nombre mem -> case mem of{
    [] -> error ("(@@): La variable "++ nombre ++" no esta en la memoria");
    ((nom, valor):ps) -> case nom == nombre of{
        True -> valor;
        False -> nombre @@ ps
    }
}
-- 2
upd :: (Nom,Int) -> Mem -> Mem
upd = \(s, n) memoria -> case memoria of{
    [] -> [(s, n)];
    (nom, valor):ps -> case nom == s of{
        True -> (s, n):ps;
        False -> [(nom, valor)] ++ upd (s, n) ps
    }
}
-- 3
eval :: Exp -> Mem -> Int
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
-- Ejecución de programas
-- 4

-- funcion auxiliar para la definicion de run en el case ASIG. recorre lista de nombres (Nom) con expresiones (Exp) y devuelve nombres con valores (Int)
aux::[(Nom, Exp)] -> Mem ->[(Nom, Int)]
aux = \expresiones memoria -> case expresiones of{
    [] -> [];
    (nom, exp):exps -> (nom, (eval exp memoria)):(aux exps memoria)
}
-- Otra funcion auxiliar que nos ayuda en run en el case ASIG. Esta se encarga de asignar para la memoria recibida, todos los valores nuevos hallados con la funcion aux
-- En resumen, se encarga de hacer todos los upd (actualizar la memoria que recibe) con los nuevos valores
asignaciones::[(Nom, Int)] -> Mem -> Mem
asignaciones = \asigs memoria -> case asigs of{
    [] -> memoria;
    (nombre, int):restoAsignaciones -> asignaciones restoAsignaciones ( upd (nombre, int) memoria )
}

run :: Prog -> Mem -> Mem
run = \pg mem -> case pg of{
    ASIG operaciones -> asignaciones (aux operaciones mem) mem;
    pg1 :> pg2 -> run pg2 (run pg1 mem);
    (IF conditional progT progF) -> case beval conditional mem of{
        True -> run progT mem;
        False -> run progF mem;
    };
    WHILE conditional prog -> case beval conditional mem of{
        True -> run (WHILE conditional prog) (run prog mem);
        False -> mem
    }
}
										
										
-- Ejemplos de programas
p0 :: Prog
p0 = ASIG [("x", I 1)] 
	:> ASIG [("x", V "x" :+ I 10)]

p1 :: Prog
p1 = ASIG [("x" , I 1), ("w", I 2)] 
     :> IF (V "w":== V "x") (ASIG [("z", I 3)]) (ASIG [("z", I 4)])

p2 :: Prog
p2 = ASIG [("x" , I 27), ("w", I 7)] 
     :> WHILE (NOT (V "x":== I 0)) (ASIG [("w", V "w":+ I 2),("x", V "x":- V "w")])

-- Programas a definir
-- 5
swap_xw :: Prog
swap_xw = ASIG [("x", V "w"), ("w", V "x")]

-- 6
fact :: Int -> Prog
fact = \n -> case n < 0 of{
    True -> undefined;
    False -> ASIG [("fact", I 1), ("i", I n )] :> WHILE (NOT (V "i" :== I 0)) ((ASIG [("fact", V "fact" :* V "i" ), ("i", V "i" :- I 1)]))
}



-- usamos la función factorial para probar el programa fact
factorial :: Int -> Int
factorial = \n -> "fact" @@ run (fact n) []

-- 8
par :: Int -> Prog
par = \n -> case n `mod` 2 of{
    0 -> ASIG [("par", I 1)];
    _ -> ASIG [("par", I 0)];
}

-- usamos la función esPar para probar el programa par
esPar :: Int -> Bool
esPar = \n -> "par" @@ (run (par n) []) == 1

-- 9
mini :: Int -> Int -> Prog
mini = \x y -> case (x - y) < 0 of{
    True -> ASIG [("min", I x)];
    False -> ASIG [("min", I y)];
}

-- usamos la función minimo para probar el programa mini
minimo :: Int -> Int -> Int
minimo = \m n -> "min" @@ (run (mini m n) [])

-- 10
fib :: Int -> Prog 
fib = \n -> case n < 1 of{
    True -> ASIG [("fib", I 1)];
    False -> ASIG [("i", I 1), ("fib", I 1), ("aux", I 1)] :> WHILE (NOT (V "i" :== I n)) ((ASIG [("fib", V "fib" :+ V "aux" ), ("aux", V "fib"), ("i", V "i" :+ I 1)]))
}

-- usamos la función fibonacci para probar el programa fib
fibonacci :: Int -> Int
fibonacci = \n -> "fib" @@ run (fib n) []
