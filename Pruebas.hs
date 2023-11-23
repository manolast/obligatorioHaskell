{-# OPTIONS_GHC -fno-warn-tabs #-}
{-#LANGUAGE GADTs#-}

module Pruebas where
import Datas
import Prog
import Data.List


--Instrucciones
--1) Descargar este archivo en la misma carpeta en que se encuentran Datas.hs y Prog.hs.
--2) Abrir este archivo con ghci.
--3) Para cada función pedida hay una serie de pruebas pi que pueden 
--   correrse por separado y devuelven True cuando el resultado es correcto.
--4) También hay una prueba que verifica todos los casos de cada función.
--5) Finalmente hay una prueba pTodo que verifica todas las pruebas de todas las funciones del entregable.
--6) Para la función extra (variables) hay pruebas al final del archivo



perm :: Eq a => [a] ->[a] ->Bool
perm = \l1 l2 -> elem l1 (permutations l2)

-- Memorias
m1 = [("x",1),("y",2),("z",3)]
m2 = upd ("w",0) m1
m3 = upd ("y",5) m2
m4 = upd ("x",0) []

-- Programas
prog1 = ASIG [("x",I 10),("z",I 20)]
prog2 = ASIG [("x",V "z"),("y",V "x")]
prog3 = ASIG [("x",V "z" :+ V "y"),("y",V "x")]
prog4 = ASIG [("x", I 27), ("w", I 7)] :> WHILE (NOT (V "x":== V "w")) (ASIG [("w", V "w":+ I 2),("x", V "x":- V "w")])
prog5 = ASIG [("m", I 0), ("n", I 1), ("i", I 10) ] :> WHILE (NOT (V "m":== V "i")) (ASIG [("w", V "w":+ V "i"),("i", V "i":- V "n")])
prog6 = IF (V "a" :== I 0) (WHILE (NOT (V "b":== V "a"))(ASIG [("b", V"b" :- I 1)])) (WHILE (NOT (V "b":== I 3))(ASIG [("b", V"b" :+ I 1)]))
prog7 = WHILE (NOT(V "x" :== V "y")) (IF (V "x" :* V "y" :== I 0) (ASIG [("x",V "y")])(ASIG [("x",V "x" :- I 1)]))

-- update
pr1 = m2 == [("x",1),("y",2),("z",3),("w",0)]
pr2 = m3 == [("x",1),("y",5),("z",3),("w",0)]
pr3 = m4 == [("x",0)]
p7 = upd ("z",7) [("x",1),("z",3),("y",2)] == [("x",1),("z",7),("y",2)]
p8 = upd ("z",7) [("x",1),("y",2)] == [("x",1),("y",2),("z",7)]
p9 = upd ("z",7) [] == [("z",7)]

pupd = pr1&&pr2&&pr3&&p7&&p8&&p9

-- @@
pr4 = "y" @@ m1 == 2
pr5 = "y" @@ m3 == 5
pr6 = "x" @@ m4 == 0
-- "w" @@ m1 ==> * Exception: (@@): La variable w no está en la Memoria

plook = pr4&&pr5&&pr6


-- eval
p10 = eval (V "x" :+ V "y") m1 == 3
p11 = eval (V "z" :* I 5) m2 == 15
p12 = eval (V "w" :- V "z" :* V "x") m3 == -3
-- eval (V "x" :- V "y") m4 ==> * Exception: (@@): La variable y no está en la Memoria

-- beval
p13 = beval (F :&& T :|| T) [] == True
p14 = beval (V "z":== V "x":+ V "w" :&& V "w":== V "x":* I 2) [("x",1),("w",2),("z",3)] == True
p15 = beval (V "x" :== I 1 :&& V "z" :== I 3) m3 == True
p16 = beval (V "x" :== I 0 :&& V "z" :== I 3) m3 == False
p17 = beval (V "x" :== I 0 :|| V "z" :== I 3) m3 == True
p18 = beval (V "w" :== I 1 :|| (V "z" :== V "x")) m3 == False
p19 = beval (NOT (V "x" :== I 1)) m2 == False
p20 = beval (NOT(V "x":== V "z")) [("x",1),("w",2),("z",3)] == True
p21 = beval (V "w" :== (V "z" :+ V "w")) m3 == False
p22 = beval (V "w" :+ I 5 :== V "z" :+ V "y" :* V "x") m2 == True
p23 = beval (I 0 :== V "z") m1 == False 
p24 = beval (V "x" :== I 1) m1 == True
-- beval (V "x" :== V "z") [] ==> * Exception: (@@): La variable x no está en la Memoria
-- beval (V "x" :== V "z" :* V w") m4 ==> * Exception: (@@): La variable z no está en la Memoria

peval = and[p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24]

-- ASIG
p25 = perm (run (ASIG [("x",V "z"),("y",V "x"),("z",V "y")])m1)([("x",3),("y",1),("z",2)])
p26 = perm (run (ASIG [("x",I 1),("y",V "x"),("z",V "w")])[("x",0),("w",1)])([("x",1),("w",1),("y",0),("z",1)])
p27 = perm (run (ASIG [("x",I 1),("y",V "x"),("z",I 2)]) [("x",0),("w",1),("z",20)])([("x",1),("w",1),("z",2),("y",0)])
p28 = perm (run (ASIG [("x",V"y":-V"x"),("y",V "x":-V"y")]) [("x",3),("y",8)])([("x",5),("y",-5)])
p29 = perm (run (ASIG [("x",I 10),("z",I 20)]) [])([("x",10),("z",20)])
p30 = perm (run (ASIG [("x",I 10),("z",I 20)]) [("z",8)])([("z",20),("x",10)])
p31 = perm (run (ASIG [("x",V"y"),("y",V "x")]) m1)([("x",2),("y",1),("z",3)])
-- run (ASIG [("x",V "y")]) [] ==> * Exception (@@): La variable y no está en la Memoria
-- run (ASIG [("z",V "x")]) [("z",20)] ==> * Exception (@@): La variable x no está en la Memoria 
-- run (ASIG [("x", I 1),("x", V "x" :+ I 10)]) [] ==> * Exception (@@): La variable x no está en la Memoria

pasig = and[p25,p26,p27,p28,p29,p30,p31]

-- :>
p32 = perm (run (prog1 :> prog2) m1) ([("x",20),("y",10),("z",20)])
p33 = perm (run (prog1 :> prog2) m2) ([("x",20),("y",10),("z",20),("w",0)])
p34 = perm (run (prog1 :> prog2) m3) ([("x",20),("y",10),("z",20),("w",0)])
p35 = perm (run (prog2 :> prog3) m3) ([("x",4),("y",3),("z",3),("w",0)])
p36 = perm (run (prog1 :> prog3) m1) ([("x",22),("y",10),("z",20)])

pcomp = p32&&p33&&p34&&p35&&p36

-- IF
p37 = perm (run (IF (NOT(V "x" :== I 0)) (ASIG[("y",I 1)]) (ASIG[("y",I 2)])) m1) ([("x",1),("y",1),("z",3)])
p38 = perm (run (IF (V "w" :== I 0) (ASIG[("y",I 1)]) (ASIG[("y",I 2)])) m3) ([("x",1),("y",1),("z",3),("w",0)])
p39 = perm (run (IF (V "w" :== I 0) (ASIG[("y",I 2 :- V "w")]) (ASIG[("y",I 2 :+ V "w")])) [("w",0),("y",10)]) ([("w",0),("y",2)])

pif = p37&&p38&&p39

-- WHILE
p40 = perm (run(WHILE (NOT(V "z" :== I 0)) (ASIG [("z" , V "z" :-  I 2)])) [("z",10)]) ([("z",0)])
p41 = perm (run(WHILE (NOT(V "x" :== I 10)) (ASIG [("z" , V "x" :+  I 2),("x" , V "x" :-  I 10)])) [("x",100)]) ([("x",10),("z",22)])
p42 = perm (run(WHILE (NOT(V "x" :== I 0)) (ASIG [("z" , I 1 :- V "z"),("x" , V "x" :-  I 1)])) [("x",10),("z",0)]) ([("x",0),("z",0)])
p43 = perm (run prog4 []) ([("x",11),("w",11)])
p46 = perm (run prog6 [("a",0),("b",8)]) ([("a",0),("b",0)])
p47 = perm (run prog6 [("a",100),("b",-88)]) ([("a",100),("b",3)])
p48 = perm (run prog7 m2) ([("x",2),("y",2),("z",3),("w",0)])

pwhile = p40&&p41&&p42&&p43&&p46&&p47&&p48

-- swap_xw
p45 = perm (run swap_xw  [("w",0),("y",10),("x",100)]) ([("w",100),("y",10),("x",0)])
p44 = perm (run swap_xw m2) ([("x",0),("y",2),("z",3),("w",1)])

pswap = p44 && p45

-- fact
pfact = factorial 0 == 1 && factorial 5 == 120 && factorial 8 == 40320 && factorial 15 == 1307674368000

-- par
ppar = esPar 2  && not(esPar 5) && esPar 100 && not(esPar 555) 

--min
pmin = minimo 5 8 == 5 && minimo 6 6 == 6 && minimo 0 7 == 0 && minimo 100 5 == 5

--fib
pfib = fibonacci 0 == 1 && fibonacci 1 == 1 && fibonacci 3 == 3 && fibonacci 8 == 34 && fibonacci 30 == 1346269

-- TODO
ptodo = pupd && plook && peval && pasig && pcomp && pif && pwhile && pswap && pfact && ppar && pmin && pfib

-- variables
-- pvars = variables p0 == ["x"] 
-- 		&& sort (variables prog1) == ["x","z"]
--  		&& sort (variables prog4) == ["w","x"] 		
-- 		&& sort (variables (prog1 :> prog4 :> p2)) == ["w","x","z"] 
-- 		&& sort (variables (IF T prog4 prog5)) == ["i","m","n","w","x"]
		