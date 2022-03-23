-- 20. Definir la funci´on esBisiesto: Num→ Bool, que indica si un a˜no es bisiesto. Un a˜no es bisiesto si es
--divisible por 400 o es divisible por 4 pero no es divisible por 100. 
esBisiesto :: Int -> Bool
esBisiesto a = (mod a 400) == 0 || ((mod a 4) == 0 && ((mod a 100) /= 0))
--ordena : (Num, Num) → (Num, Num), que dados dos enteros los ordena de menor a mayor.
ordena :: (Int,Int) -> (Int,Int)
ordena (a,b) = ((min a b),(max a b))