--ejercicio 8

sumaTotal :: [[Integer]] -> Integer
sumaTotal [] = 0
sumaTotal ((fila):matriz) = sumatoria (fila) + sumaTotal (matriz)

sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria (xs)

--2.
cantidadDeApariciones :: Integer -> [[Integer]] -> Integer
cantidadDeApariciones k [] = 0
cantidadDeApariciones k ((fila):matriz) = aparicionesEnFila k (fila) + cantidadDeApariciones k (matriz)

aparicionesEnFila :: Integer -> [Integer] -> Integer
aparicionesEnFila k [] = 0
aparicionesEnFila k (x:xs) |k==x = 1 + aparicionesEnFila k (xs)
                           |otherwise = aparicionesEnFila k (xs)

--5.
multiplicarPorEscalar :: Integer -> [[Integer]] -> [[Integer]]
multiplicarPorEscalar k [] = []
multiplicarPorEscalar k ((fila):matriz) = [multiplicarFila k (fila)] ++ multiplicarPorEscalar k (matriz)

multiplicarFila :: Integer -> [Integer] -> [Integer]
multiplicarFila k [] = []
multiplicarFila k (x:xs) = [k*x] ++ multiplicarFila k (xs)

--7.
iésimaFila :: Integer -> [[a]] -> [a]
iésimaFila i [] = []
iésimaFila i ((fila):matriz) = contadorFila i ((fila):matriz) 0

contadorFila :: Integer -> [[a]] -> Integer -> [a]
contadorFila i [] k = []
contadorFila i ((fila):matriz) k |i==k = fila
                                 |otherwise = contadorFila i (matriz) (k+1)

--8.
iésimaColumna :: Integer -> [[a]] -> [a]
iésimaColumna j [] = []
iésimaColumna j ((fila):matriz) = contadorColumna j (fila) 0 ++ iésimaColumna j (matriz)

contadorColumna :: Integer -> [a] -> Integer -> [a]
contadorColumna i [] k = []
contadorColumna i (x:xs) k |i==k = [x]
                           |otherwise = contadorColumna i (xs) (k+1)