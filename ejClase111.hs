data Polinomio = Mono Float Integer
               | Suma Polinomio Polinomio
               | Producto Polinomio Polinomio deriving Eq
instance Show Polinomio where
  show (Mono 0 n) = " 0"
  show (Mono a 0) = show a
  show (Mono a 1) = show a ++ " x"
  show (Mono a n) = show a ++ " x^" ++ show n
--  show (Suma p (Mono a n)) = show' (Suma p (Mono a n))

  show (Producto p q) = "(" ++ show p ++ ") * (" ++ show q ++ ")"

  show (Suma p (Mono a n))
        | a <  0 = show p ++ " - " ++ show (Mono (abs a) n) 
        | a >  0 = show p ++ " + " ++ show (Mono a n)
--        | a == 0 = show p
        | otherwise = show p

instance Num Polinomio where
  p + q = Suma p q
  p * q = Producto p q
  negate p = opuesto p
  abs p = error "no hay por ahora"
  signum p = error "no hay por ahora"
  fromInteger n = Mono (fromInteger n) 0

eval :: Polinomio -> Float -> Float
eval (Mono a n) x = a * (x ^ n)
eval (Suma p1 p2) x = eval p1 x + eval p2 x 
eval (Producto p1 p2) x = (eval p1 x) * (eval p2 x)

coefs :: Polinomio -> [Float]
coefs (Mono a 0) = [a]
coefs (Mono a n) = 0 : coefs (Mono a (n-1))
coefs (Suma p q) = sumaListas (coefs p) (coefs q)
coefs (Producto p q) = productoListas (coefs p) (coefs q)

polinomioSuma :: Polinomio -> Polinomio -> Polinomio
polinomioSuma s1 s2 = polinomioDe (eliminarCeros(coefs (Suma s1 s2))) 

polinomioDe :: [Float] -> Polinomio
polinomioDe [] = Mono 0 0
polinomioDe (x:xs) = sumaMono (reverse(x:xs)) (toInteger(length(x:xs))-1)

sumaMono :: [Float] -> Integer -> Polinomio
sumaMono (x:xs) n |    n == 0 = (Mono x 0)
                  | otherwise = (sumaMono xs (n-1)) + (Mono x n)

polinomioProducto :: Polinomio -> Polinomio -> Polinomio
polinomioProducto h1 h2 = polinomioDe (eliminarCeros(coefs (Producto h1 h2)))

sumaListas :: [Float] -> [Float] -> [Float]
sumaListas [] ys = ys
sumaListas xs [] = xs
sumaListas (x:xs) (y:ys) = (x + y) : (sumaListas xs ys)

productoListas :: [Float] -> [Float] -> [Float]
productoListas [] _ = []
productoListas _ [] = []
productoListas (x:xs) ys = sumaListas xys xsys
      where xys = cteLista x ys
            xsys = 0 : productoListas xs ys

cteLista :: Float -> [Float] -> [Float]
cteLista n [] = []
cteLista n (x:xs) = (x * n) : (cteLista n xs)

opuesto :: Polinomio -> Polinomio
opuesto (Mono a n) = Mono (-a) n
opuesto (Suma p q) = Suma (opuesto p) (opuesto q)
opuesto (Producto p q) = Producto (opuesto p) q

eliminarCeros :: [Float] -> [Float]
eliminarCeros [] = []
--eliminarCeros [0] = [0]
eliminarCeros xs | (last xs) == 0 = eliminarCeros (init xs)
                 | otherwise = xs

gradoPol :: Polinomio -> Integer
gradoPol p = toInteger(length (eliminarCeros (coefs p)) - 1)

coefPpal :: Polinomio -> Float
coefPpal p = last(coefs p)


construirListaDe :: Integer -> Float -> [Float]
construirListaDe cont valor
               | cont == 0  = []
               | otherwise  = valor : construirListaDe (cont - 1) valor


       

--divP :: Polinomio -> Polinomio -> Polinomio
divP p1 p2 
  | gradoPol p2 < 0 = error "no es un numero, o infinito"
  | gradoPol p1 < gradoPol p2 = Mono 0 0
  | otherwise = polinomioDe(divPol p1 p2)

divPol :: Polinomio -> Polinomio -> [Float]
divPol p1 p2 = divPoli (reverse(coefs p1)) (reverse(coefs p2) ++ (construirListaDe difg 0)) (difg + 1)  
            where difg =  toInteger(length(coefs p1) - length(coefs p2))

divPoli :: [Float] -> [Float] -> Integer -> [Float]
divPoli (x:xs) (y:ys) n 
  | n==0 = []
  |otherwise = (divPoli (tail(restoParcial) ++ [0]) (y:ys) (n-1) ) ++ [ x/y ]  
                  where restoParcial = (restaLista (x:xs) (y:ys)) 
       
restaLista (x:xs) (y:ys) = sumaListas (x:xs) (cteLista ((-1)*(x/y)) (y:ys))

modP :: Polinomio -> Polinomio -> Polinomio
modP q1 q2 
  | gradoPol q1 < gradoPol q2 = q1 
  | otherwise = polinomioSuma q1 (opuesto (polinomioProducto (divP q1 q2) q2))



euclidesP :: Polinomio -> Polinomio -> Polinomio
euclidesP q r 
--  | coefs r == (Mono 0 0) = q
  |  gradoPol r <= 0  = q
  | otherwise = euclidesP  r (modP q r) 


{-  
*Main> let p = ( Mono 1 1) + ( Mono ( 2) 3) + ( Mono (2) 5)
*Main> let q = ( Mono 2 0) + ( Mono ( -3) 1) + ( Mono 1 2)
*Main> divP p q
36.0 + 16.0 x + 6.0 x^2 + 2.0 x^3
*Main> modP p q
-72.0 + 77.0 x
*Main> let h = modP p q
*Main> h
-72.0 + 77.0 x
*Main> divP q h
^CInterrupted.
*Main> q
2.0 - 3.0 x + 1.0 x^2
*Main> h
-72.0 + 77.0 x
*Main> divP q h
^CInterrupted.
*Main> let h = ( Mono (-72) 0) + ( Mono 77 1) 
*Main> h
-72.0 + 77.0 x
*Main> divP q h
-2.681734e-2 + 1.2987013e-2 x



-}
