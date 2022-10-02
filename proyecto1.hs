-- ejercicio 1a
esCero :: Int -> Bool
esCero 0 = True
esCero x = x== 0 

-- ejercicio 1b
esPositivo :: Int -> Bool
esPositivo 1 = True
esPositivo x = x > 0

-- ejercicio 1c
esVocal :: Char -> Bool
esVocal ' '= True
esVocal a = a == 'a' || a == 'e' || a == 'i' || a == 'o' || a == 'u'

-- ejercicio 2a
paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) = x && paratodo xs

-- ejercicio 2b
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- ejercicio 2c
productoria ::[Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- ejercicio 2d
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- ejercicio 2e
promedio :: [Int] -> Int
promedio [] = 0
promedio (x:xs) = (sumatoria (x:xs)`div`(length xs))

-- ejercicio 3
pertenece :: Int -> [Int] -> Bool
pertenece 0 [] = True
pertenece c (x:xs) = c `elem` (x:xs)

-- ejercicio 4a
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] function = True
paratodo' (x:xs) function = (function x) && paratodo' xs function

-- ejercicio 4b
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] function = False
existe' (x:xs) function = (function x) || existe' xs function

-- ejercicio 4c
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] function = 0
sumatoria' (x:xs) function = (function x) + sumatoria' xs function

-- ejercicio 4d
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] function = 1
productoria' (x:xs) function = (function x) * productoria' xs function

-- ejercicio 5
paratodo'' :: [Bool] -> Bool
paratodo'' xs = paratodo' xs id 

-- ejercicio 6a
todosPares :: [Int] -> Bool
todosPares x = paratodo' x even

-- ejercicio 6b
hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n xs = existe' xs (p n)

p :: Int -> Int -> Bool
p n x = x `mod`n == 0 

-- ejercicio 6c
sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' [0..n-1] (^2)

-- ejercicio 6d
factorial' :: Int -> Int
factorial' n = productoria' [1..n-1] id

-- ejercicio 6e
multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria' (filter even xs) (*1)

-- ejercicio 7
-- la funcíon map le aplica f a una lista de xs y devuelve una lista con el
-- resultado de aplicarle f a cada elemento de xs
-- map succ [1, -4, 6, 2, -8], donde succ n = n+1 va a devolver una lista con
-- los sucesores de cada uno de los numeros en la lista. [2,-3,7,3,-7]
-- la funcion filter aplicada a un predicado y una lista devuelve una lista
-- de elementos que satisfacen ese predicado.
-- la expresion filter esPositivo [1, -4, 6, 2, -8] va a devolver la lista 
-- que solo contenga los positivos. [1,6,2]

-- ejercicio 8
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = x*2 : duplica xs

duplica' :: [Int] -> [Int]
duplica' xs = map (\x -> x * 2) xs

-- ejercicio 9a
soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) 
                | (x)`mod` 2 == 0 = (x) : soloPares xs
                | (x)`mod` 2 == 1 = soloPares xs

-- ejercicio 9b
soloPares' :: [Int] -> [Int]
soloPares' xs = filter even xs


-- ejercicio 10
primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA _ [] = []
primIgualesA x' (x:xs)
                      | x' == x = x : primIgualesA x' xs
                      | otherwise = []

primIgualesA' :: Eq a => a -> [a] -> [a]
primIgualesA' a xs = takeWhile (==a) xs


-- ejercicio 11
primIguales :: (Eq a) => [a] -> [a]
primIguales [] = []
primIguales (x:y:xs) 
                  | x == xs!!0 = x : primIguales xs
                  | otherwise = []


primIguales' :: (Eq a) => [a] -> [a]
primIguales' xs = primIgualesA' (xs!!0) xs


-- ejercicio 12
cuantGen :: (b -> b -> b) -> b -> [a] -> (a -> b) -> b
cuantGen op z [] t = z
cuantGen op z (x:xs) t =  (t x) `op` (cuantGen op z xs t)


cuantGenParatodo :: [a] -> (a -> Bool) -> Bool
cuantGenParatodo xs t = cuantGen (&&) True xs t 

cuantGenExiste :: [a] -> (a -> Bool) -> Bool
cuantGenExiste xs t = cuantGen (||) False xs t 

cuantGenSumatoria :: [a] -> (a -> Int) -> Int
cuantGenSumatoria xs t = cuantGen (+) 0 xs t 

cuantGenProductoria :: [a] -> (a -> Int) -> Int
cuantGenProductoria xs t = cuantGen (*) 1 xs t 