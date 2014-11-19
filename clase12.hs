funcion2 :: Bool -> (Integer -> Integer)
funcion2 True = fact
funcion2 False = fib

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1) 

sumaproducida:: Integer ->  ([Integer] -> Integer)
sumaproducida n	| n > 10 = sum
		|otherwise = product


data Eleccion = Piedra |Papel | Tijera | Lagarto | Spock deriving Show

ganaA :: Eleccion -> Eleccion -> Bool
ganaA Piedra Tijera = True
ganaA Tijera Papel = True
ganaA Papel  Piedra = True
ganaA _ _ = False

ganaA' :: Eleccion -> Eleccion -> Bool
ganaA' Lagarto _ = True
ganaA' Spock _ = True
ganaA' x y = ganaA x y
ganaA' _ _ = False

juego :: Eleccion -> Eleccion -> (Eleccion -> Eleccion -> Bool) -> Eleccion
juego e1 e2 regla | regla e1 e2 = e1
                  | regla e2 e1 = e2
                  | otherwise = error "empate :(" 

aplicar2Veces :: (a -> a) -> a -> a
aplicar2Veces f x = f (f x)

magia :: (a -> a) -> [a] -> [a]
magia _ [] = []
magia f (x:xs) = f x : magia f xs

suma x y = x + y
suc = suma 1

{-
Copyright (C) 2009-2011 Intel Corporation. All rights reserved.
Intel(R) Inspector XE 2011 (build 186554)
Copyright (C) 2009-2011 Intel Corporation. All rights reserved.
Intel(R) VTune(TM) Amplifier XE 2011 (build 186533)
acuellar@ws2:~$ cd Taller
acuellar@ws2:~/Taller$ ghci
GHCi, version 7.4.1: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :l ejClase111
[1 of 1] Compiling Main             ( ejClase111.hs, interpreted )
Ok, modules loaded: Main.
*Main> let q = ( Mono (-1) 0) + ( Mono ( 1) 4)
*Main> q
-1.0 + 1.0 x^4
*Main> let p = ( Mono 1 1) + ( Mono ( 2) 3) + ( Mono (1) 5)
*Main> p
1.0 x + 2.0 x^3 + 1.0 x^5
*Main> divP p q
0 + 1.0 x
*Main> modP p q
0 + 2.0 x + 2.0 x^3
*Main> euclidesP p q
^CInterrupted.
*Main> 
Leaving GHCi.
acuellar@ws2:~/Taller$ ghci
GHCi, version 7.4.1: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :l clase5f
[1 of 1] Compiling Main             ( clase5f.hs, interpreted )
Ok, modules loaded: Main.
*Main> mcd2 4 6
2
*Main> mcd2 6 15
3
*Main> mcd2 15  4
1
*Main> 
*Main> 
[1]+  Stopped                 ghci
acuellar@ws2:~/Taller$ ghci
GHCi, version 7.4.1: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :l ejClase111
[1 of 1] Compiling Main             ( ejClase111.hs, interpreted )
Ok, modules loaded: Main.
*Main> let q = ( Mono 2 0) + ( Mono ( -3) 1) + ( Mono 1 2)
*Main> q
2.0 - 3.0 x + 1.0 x^2
*Main> let p = Suma ( Mono 1 0) ( Mono 2 1)
*Main> p
1.0 + 2.0 x
*Main> euclidesP q p
^CInterrupted.
*Main> euclidesP  p q
^CInterrupted.
*Main> :l clase12
[1 of 1] Compiling Main             ( clase12.hs, interpreted )

clase12.hs:1:31: parse error on input `)'
Failed, modules loaded: none.
Prelude> :l clase12
[1 of 1] Compiling Main             ( clase12.hs, interpreted )

clase12.hs:2:17:
    Couldn't match expected type `(Integer, Integer)'
                with actual type `Integer -> Integer'
    In the expression: fact
    In an equation for `funcion2': funcion2 True = fact
Failed, modules loaded: none.
Prelude> :l clase12
[1 of 1] Compiling Main             ( clase12.hs, interpreted )
Ok, modules loaded: Main.
*Main> (funcion2 True) 8
40320
*Main> (funcion2 True) 4
24
*Main> sumaproducida 5

<interactive>:14:1: Not in scope: `sumaproducida'
*Main> :r
[1 of 1] Compiling Main             ( clase12.hs, interpreted )
Ok, modules loaded: Main.
*Main> sumaproducida 8

<interactive>:16:1:
    No instance for (Show ([a10] -> a10))
      arising from a use of `print'
    Possible fix: add an instance declaration for (Show ([a10] -> a10))
    In a stmt of an interactive GHCi command: print it
*Main> sumaproducida 8 [1,2,3,4]
24
*Main> sumaproducida 8 [1,2,3,4,5]
120
*Main> sumaproducida 18 [1,2,3,4,5]
15
*Main> :r
[1 of 1] Compiling Main             ( clase12.hs, interpreted )
Ok, modules loaded: Main.
*Main> sumaproducida 18 [1,2,3,4,5]
15
*Main> sumaproducida 8 [1,2,3,4,5]
120
*Main> sumaproducida 8 [1]
1
*Main> sumaproducida 8 []
1
*Main> sumaproducida 18 []
0
*Main> :r
[1 of 1] Compiling Main             ( clase12.hs, interpreted )
Ok, modules loaded: Main.
*Main> ganaA Piedra  Piedra
True
*Main> :r
[1 of 1] Compiling Main             ( clase12.hs, interpreted )
Ok, modules loaded: Main.
*Main> ganaA Piedra  Piedra
False
*Main> juego Piedra Tijera ganaA
Piedra
*Main> :r
[1 of 1] Compiling Main             ( clase12.hs, interpreted )
Ok, modules loaded: Main.
*Main> aplicar2Veces not True
True
*Main> aplicar2Veces (not True)

<interactive>:33:16:
    Couldn't match expected type `a0 -> a0' with actual type `Bool'
    In the return type of a call of `not'
    Probable cause: `not' is applied to too many arguments
    In the first argument of `aplicar2Veces', namely `(not True)'
    In the expression: aplicar2Veces (not True)
*Main> :r
[1 of 1] Compiling Main             ( clase12.hs, interpreted )

clase12.hs:28:1:
    Warning: Pattern match(es) are overlapped
             In an equation for ganaA': ganaA' _ _ = ...
Ok, modules loaded: Main.
*Main> magia not [True, True, False, False]
[False,False,True,True]
*Main> magia reverse [[1,2,3],[5,6,7]]
[[3,2,1],[7,6,5]]
*Main> magia id [[1,2,3,4,5,6]]
[[1,2,3,4,5,6]]
*Main> magia toLower "HOLA"

<interactive>:38:7: Not in scope: `toLower'
*Main> magia toLower ['A','B']

<interactive>:39:7: Not in scope: `toLower'
*Main> magia toLower [['A','B']]

<interactive>:40:7: Not in scope: `toLower'
*Main> (\x -> x +10) 20
30
*Main> (\radio -> (4*pi*radio**3)/3 6378

<interactive>:42:34: parse error (possibly incorrect indentation)
*Main> (\radio -> (4 * pi * radio ** 3)/3 6378

<interactive>:43:40: parse error (possibly incorrect indentation)
*Main> (\radio -> (4 * pi * radio ** 3)/3 

<interactive>:44:35: parse error (possibly incorrect indentation)
*Main> (\radio -> (4 * pi * radio ** 3) / 38

<interactive>:45:38: parse error (possibly incorrect indentation)
*Main> (\radio -> (4 * pi * radio ** 3) / 3) 8
2144.660584850632
*Main> max 4 5
5
*Main> (max 4) 5
5
*Main> max 4 

<interactive>:49:1:
    No instance for (Show (a0 -> a0))
      arising from a use of `print'
    Possible fix: add an instance declaration for (Show (a0 -> a0))
    In a stmt of an interactive GHCi command: print it
*Main> :t suc

<interactive>:1:1:
    Not in scope: `suc'
    Perhaps you meant one of these:
      `sum' (imported from Prelude), `succ' (imported from Prelude)
*Main> :r
[1 of 1] Compiling Main             ( clase12.hs, interpreted )

clase12.hs:28:1:
    Warning: Pattern match(es) are overlapped
             In an equation for ganaA': ganaA' _ _ = ...
Ok, modules loaded: Main.
*Main> :t suc
suc :: Integer -> Integer
*Main> suc 10
11
*Main> suc

<interactive>:54:1:
    No instance for (Show (Integer -> Integer))
      arising from a use of `print'
    Possible fix:
      add an instance declaration for (Show (Integer -> Integer))
    In a stmt of an interactive GHCi command: print it
*Main> :t suma
suma :: Num a => a -> a -> a
*Main> (>10) 8
False
*Main> (10>) 8
True
*Main> (>) 8

<interactive>:58:1:
    No instance for (Show (a0 -> Bool))
      arising from a use of `print'
    Possible fix: add an instance declaration for (Show (a0 -> Bool))
    In a stmt of an interactive GHCi command: print it
*Main> (>) 8 10
False
*Main> map (+25) [1,2,3]
[26,27,28]
*Main> count (==0) [1,0,2,0,0]

<interactive>:61:1: Not in scope: `count'
*Main> counter (==0) [1,0,2,0,0]

<interactive>:62:1: Not in scope: `counter'
*Main> 

-}

