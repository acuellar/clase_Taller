data Polinomio = Mono Float Integer
               | Suma Polinomio Polinomio
               | Producto Polinomio Polinomio deriving Eq
instance Show Polinomio where
{-  show (Mono a n) | n == 0 = show a
                  | n == 1 = show a ++ " x"
                  | a >= 0 =  show a ++ " x^" ++ show n
                  | a <  0 = " - " ++ show (Mono (abs a) n)
  -}

  show (Mono 0 n) = "0"
  show (Mono a 0) = show a
  show (Mono a 1) = show a ++ " x"
  show (Mono a n) = show a ++ " x^" ++ show n
--  show (Suma p (Mono a n)) = show' (Suma p (Mono a n))

  show (Producto p q) = "(" ++ show p ++ ") * (" ++ show q ++ ")"
--  show (Suma  p (Mono a n)) =  show (Suma (Mono a n) p)
{-
  show (Suma (Mono a n) p)
        | a <  0 = show p ++ " - " ++ show (Mono (abs a) n) 
        | a >  0 = show p ++ " + " ++ show (Mono a n)
--        | a == 0 = show p
        | otherwise = show p
-}        
  show (Suma p (Mono a n))
        | a <  0 = show p ++ " - " ++ show (Mono (abs a) n) 
        | a >  0 = show p ++ " + " ++ show (Mono a n)
--        | a == 0 = show p
        | otherwise = show p
--  show (Suma p q ) = show p ++ " + " ++ show q
{-
show' (Suma (Mono a n) (Mono b m))
        | a < 0 && b > 0 = " - " ++ show (Mono (abs a) n) ++ " + " ++ show (Mono b m)
        | a > 0 && b < 0 = show (Mono a n) ++ " - " ++ show (Mono (abs b) m)
        | a < 0 && b < 0 = " - " ++ show (Mono (abs a) n) ++ " - " ++ show (Mono (abs b) m)
        | a == 0 = show (Mono b m) 
        | b == 0 = show (Mono a n)
        | a == 0 && b == 0 = show (0)
        | otherwise = show (Mono a n) ++ " + " ++ show (Mono b m)

showSum (Mono a n) | a < 0 = " - " ++ (show (abs a)) ++ " x^" ++ (show n)
                   | otherwise = show (Mono a n)
-}
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
polinomioSuma p q = sumaMono (reverse(coefs (Suma p q))) (toInteger(length(coefs (Suma p q)))-1)

sumaMono :: [Float] -> Integer -> Polinomio
sumaMono (x:xs) n |    n == 0 = (Mono x 0)
                  | otherwise = (sumaMono xs (n-1)) + (Mono x n)

polinomioProducto :: Polinomio -> Polinomio -> Polinomio
polinomioProducto p q = sumaMono (reverse(coefs (Producto p q))) (toInteger(length(coefs (Producto p q)))-1)

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

divP :: Polinomio -> Polinomio -> Polinomio
divP p q = sumaMono (reverse(divPol p q)) (toInteger(length(divPol p q))-1)

divPol p q = divPoli (coefs p) (coefPpal p) (gradoPol p) (coefs q) (coefPpal q) (gradoPol q)

divPoli lp cpp gp lq cpq gq
  |gp < gq = []
  |otherwise = (divPoli  (restaLista lp cpp gp lq cpq gq) (last (restaLista lp cpp gp lq cpq gq)) (toInteger (length (restaLista lp cpp gp lq cpq gq)-1)) lq cpq gq ) ++ [(cpp / cpq)]  

restaLista lp cpp gp lq cpq gq = eliminarCeros (sumaListas lp (cteLista ((-1)*(cpp / cpq)) ((construirListaDe (gp - gq) 0) ++ lq )))

construirListaDe :: Integer -> Float -> [Float]
construirListaDe cont valor
               | cont == 0  = []
               | otherwise  = valor : construirListaDe (cont - 1) valor


modP :: Polinomio -> Polinomio -> Polinomio
modP p q = polinomioSuma p (opuesto (polinomioProducto (divP p q) q))


euclidesP :: Polinomio -> Polinomio -> Polinomio
euclidesP q r 
--  | coefs r == (Mono 0 0) = q
  |  gradoPol r == (-1)  = q
  | otherwise = euclidesP  r (modP q r) 

{-  
euclidesP :: Polinomio -> Polinomio -> (Polinomio, Polinomio, Polinomio)
euclidesP 0 b = (b, 0, 1)
euclidesP a b = (d, m - (b `divP` a) * n, n)
  where
     (d, n, m) = euclidesP (b `modP` a) a

*Main> let q = ( Mono 2 0) + ( Mono ( -3) 1) + ( Mono 1 2)
*Main> let p = Suma ( Mono 1 0) ( Mono 2 1)
*Main> q
-3.0 x + 2.0 + 1.0 x^2
*Main> p
2.0 x + 1.0
*Main> polinomioProducto p q
2.0 + 1.0 x - 5.0 x^2 + 2.0 x^3
*Main> polinomioSuma p q
3.0 - 1.0 x + 1.0 x^2
*Main> p + q
2.0 x + 1.0 + -3.0 x + 2.0 + 1.0 x^2
*Main> let q = ( Mono (-1) 0) + ( Mono ( 1) 4)
*Main> let p = ( Mono 1 1) + ( Mono ( 2) 3) + ( Mono (1) 5)
*Main> divP p q
0 + 1.0 x
*Main> p
1.0 x + 2.0 x^3 + 1.0 x^5
*Main> q
-1.0 + 1.0 x^4
*Main> modP p q
0 + 2.0 x + 2.0 x^3
*Main> euclidesP p q
-}
