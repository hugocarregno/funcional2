-- Realizado por Carreño Hugo

{-2.1. Definir una función siguiente, que al invocarla con un número cualquiera me devuelve el resultado de sumar a ese número el 1.-}

siguiente :: Integer -> Integer
siguiente = (+1)

{-2.2. Definir la función mitad que al invocarla con un número cualquiera me devuelve la mitad de
dicho número -}

mitad :: Float -> Float
mitad = (/2)

{-2.3. Definir una función inversa, que invocando a la función con un número cualquiera me devuelva su inversa.-}

inversa :: Float -> Float
inversa = (1/)

{-2.4. Definir una función triple, que invocando a la función con un número cualquiera me devuelva el triple del mismo.-}

triple :: Integer -> Integer
triple = (*3)

{-2.5. Definir una función esNumeroPositivo, que invocando a la función con un número cualquiera me devuelva true si el número es positivo y false en caso contrario.-}

esNumeroPositivo :: Float -> Bool
esNumeroPositivo = (>0)

{-3.1. Resolver la función del ejercicio 1.2 esMultiploDe/2, utilizando aplicación parcial y composición-}

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = (( == 0 ).( x `mod` )) y

{- 3.2. Resolver la función del ejercicio 1.5 esBisiesto/ 1, utilizando aplicación parcial y composición-}

esBisiesto :: Int -> Bool
esBisiesto anio= ((esMultiploDe 400) anio) || (((esMultiploDe 4) anio) && (((not).( esMultiploDe 100)) anio))

{- 3.3. Resolver la función inversaRaizCuadrada/ 1, que dado un número n devuelve la inversa de su raíz cuadrada.-}

inversaRaizCuadrada :: Float -> Float
inversaRaizCuadrada = inversa.sqrt

{- 3.4. Definir una función incrementMCuadradoN , que invocándola con 2 números n y m, incrementa un valor m al cuadrado de n-}

incrementMCuadradoN :: Integer -> Integer -> Integer
incrementMCuadradoN n m = (+(n*n)) m

{- 3.5. Definir una función esResultadoPar /2, que invocándola con número n y otro m, devuelve true si el resultado de elevar n a m es par.-}

esResultadoPar :: Integer -> Integer -> Bool
esResultadoPar n m = even (n^m)

{- 4.1. Definir las funciones fst3 , snd3 , trd3 , que dada una tupla de 3 elementos devuelva el elemento correspondiente-}

fst3 :: (Integer, Integer, Integer) -> Integer
fst3 (a,b,c) = fst (a,b)
snd3 :: (Integer, Integer, Integer) -> Integer
snd3 (a,b,c) = snd (a,b)
trd3 :: (Integer, Integer, Integer) -> Integer
trd3 (a,b,c) = snd (b,c)

{- 4.2. Definir la función aplicar, que recibe como argumento una tupla de 2 elementos con funciones y un entero, me devuelve como resultado una tupla con el resultado de aplicar el elemento a cada una de la funciones-}

aplicar :: ((Integer -> Integer) ,(Integer -> Integer)) -> Integer -> (Integer, Integer)
aplicar (a,b) n = (a(n),b(n)) 

{- 4.3. Definir la función cuentaBizarra, que recibe un par y: si el primer elemento es mayor al segundo devuelve la suma, si el segundo le lleva más de 10 al primero devuelve la resta 2do – 1ro, y si el segundo es más grande que el 1ro pero no llega a llevarle 10, devuelve el producto -}

cuentaBizarra :: (Integer, Integer) -> Integer
cuentaBizarra (a,b) = if(fst (a,b)  > snd (a,b) ) then (fst (a,b) + snd (a,b)) else if(snd(a,b) > (fst (a,b)+10)) then snd(a,b) - fst(a,b) else fst(a,b) * snd(a,b)

{- 4.4. Representamos las notas que se sacó un alumno en dos parciales mediante un par (nota1,nota2), p.ej. un 2 en el 1ro y un 7 en el 2do se representan mediante el par (2,7). A partir de esto:
Definir la función esNotaBochazo , recibe un número y devuelve True si no llega a 4, False en caso contrario. No vale usar guardas.
Definir la función aprobo , recibe un par e indica si una persona que se sacó esas notas aprueba.
Usar esNotaBochazo.
Definir la función promociono , que indica si promocionó, para eso tiene las dos notas tienen que sumar al menos 14 y además haberse sacado 6 en cada parcial.-}

esNotaBochazo :: Float -> Bool
esNotaBochazo = (<4)
aprobo :: (Float, Float) -> Bool
aprobo (a,b) = not((esNotaBochazo (fst (a,b))) || (esNotaBochazo(snd (a,b))))
promociono :: (Float, Float) -> Bool
promociono (a,b) = (fst(a,b)) >= 6 && (snd(a,b)) >= 6 && ((fst(a,b)) + (snd(a,b))) >= 14 

{- 4.5 Siguiendo con el dominio del ejercicio anterior, tenemos ahora dos parciales con dos recuperatorios, lo representamos mediante un par de pares ((parc1,parc2),(recup1,recup2)). Si una persona no rindió un recuperatorio, entonces ponemos un "-1" en el lugar correspondiente.
Observamos que con la codificación elegida, siempre la mejor nota es el máximo entre nota del parcial y nota del recuperatorio. Considerar que vale recuperar para promocionar. En este ejercicio vale usar las funciones que se definieron para el anterior.
Definir la función notasFinales que recibe un par de pares y devuelve el par que corresponde a las notas finales del alumno para el 1er y el 2do parcial -}

notasFinales :: ((Float,Float),(Float,Float)) -> (Float,Float)
notasFinales ((parc1,parc2),(recup1,recup2))= (max parc1 recup1 , max parc2 recup2)

{- Definir la función recuperoDeGusto que dado el par de pares que representa a un alumno, devuelve True si el alumno, pudiendo promocionar con los parciales (o sea sin recup.), igual rindió al menos un recup. Vale definir funciones auxiliares. Hacer una definición que no use pattern matching, en las eventuales funciones auxiliares tampoco; o sea, manejarse siempre con fst y snd.-}

recuperoDeGusto :: ((Float,Float),(Float,Float)) -> Bool
recuperoDeGusto ((a,b),(c,d))= promociono (fst((a,b),(c,d))) && not (snd((c,d)) == (-1))

{- 4.6. Definir la función esMayorDeEdad , que dada una tupla de 2 elementos persona, edad) me devuelva True si es mayor de 21 años y False en caso contrario.-}

esMayorDeEdad :: (String, Integer) -> Bool
esMayorDeEdad (a,b)=  (<(snd(a,b))) 21 

{- 4.7. Definir la función calcular, que recibe una tupla de 2 elementos, si el primer elemento es par lo duplica, sino lo deja como esta y con el segundo elemento en caso de ser impar le suma 1 y si no deja esté último como esta.-}
primeroPar :: (Integer, Integer) -> Integer
primeroPar (a,b) | even(fst(a,b)) = a * 2
                 | otherwise = a

segundoImpar :: (Integer, Integer) -> Integer
segundoImpar (a,b) | odd(snd(a,b)) = b + 1
                   | otherwise = b

calcular :: (Integer,Integer) -> (Integer,Integer)
calcular (a,b) = (primeroPar(a,b),segundoImpar(a,b))

main = do
  print("ejercicio 2.1 siguiente n")
  print(siguiente 3)
  print("ejercicio 2.2 mitad n")
  print(mitad 10)
  print("ejercicio 2.3 inversa n")
  print(inversa 4)
  print("ejercicio 2.4 triple n")
  print(triple 5)
  print("ejercicio 2.5 esNumeroPositivo n")
  print(esNumeroPositivo (-5))
  print("ejercicio 3.1 esMultiploDe n m")
  print(esMultiploDe 4 2)
  print("ejercicio 3.2 esBisiesto n")
  print(esBisiesto 2020)
  print("ejercicio 3.3 inversaRaizCuadrada n")
  print(inversaRaizCuadrada 4)
  print("ejercicio 3.4 incrementMCuadradoN n m")
  print(incrementMCuadradoN 3 2)
  print("ejercicio 3.5 esResultadoPar n m")
  print(esResultadoPar 2 5)
  print("ejercicio 4.1 fst3(a,b,c) ")
  print(fst3(4,5,6))
  print("ejercicio 4.1 snd3(a,b,c) ")
  print(snd3(4,5,6))
  print("ejercicio 4.1 trd3(a,b,c) ")
  print(trd3(4,5,6))
  print("ejercicio 4.2 aplicar(a,b) ")
  print(aplicar(siguiente,triple) 3)
  print("ejercicio 4.3 cuentaBizarra(a,b) ")
  print(cuentaBizarra(5,29))
  print("ejercicio 4.4 esNotaBochazo 3 ")
  print(esNotaBochazo 3)
  print("ejercicio 4.4 aprobo (2,7) ")
  print(aprobo (2,7))
  print("ejercicio 4.4 promociono (7,7) ")
  print(promociono (7,7))
  print("ejercicio 4.4 aproboprimerexamen (5,8)")
  print(esNotaBochazo (fst(5,8)))
  print("ejercicio 4.5 notasFinales((parc1,parc2),(recup1,recup2))")
  print(notasFinales((2,2),(6,3)))
  print("ejercicio 4.5 recursa ((2,7),(6,-1))")
  print(not(aprobo(notasFinales((2,7),(6,-1)))))
  print("ejercicio 4.5 recuperoprimerparcial((2,7),(6,-1))")
  print(not ((fst(snd((2,7),(6,-1)))) == (-1)))
  print("recuperoDeGusto((7,8),(6,3))")
  print(recuperoDeGusto((7,8),(6,3)))
  print("ejercicio 4.6 esMayorDeEdad(a,b) ")
  print(esMayorDeEdad("juan",29))
  print("ejercicio 4.7 calcular(a,b)")
  print(calcular(4,5))