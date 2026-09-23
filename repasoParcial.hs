--ejercicio 8

sumaTotal :: [[Integer]] -> Integer
sumaTotal [] = 0
sumaTotal ((fila):matriz) = sumatoria (fila) + sumaTotal (matriz)

sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria (xs)