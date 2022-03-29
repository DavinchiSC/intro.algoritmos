-- 20. Definir la funci´on esBisiesto: Num→ Bool, que indica si un a˜no es bisiesto. Un a˜no es bisiesto si es
--divisible por 400 o es divisible por 4 pero no es divisible por 100. 
esBisiesto :: Int -> Bool
esBisiesto a = (mod a 400) == 0 || ((mod a 4) == 0 && ((mod a 100) /= 0))
--ordena : (Num, Num) → (Num, Num), que dados dos enteros los ordena de menor a mayor.
ordena :: (Int,Int) -> (Int,Int)
ordena (a,b) = ((min a b),(max a b))

segundo3 :: (Int,Int,Int) -> Int
segundo3 (a,b,c) = b 

rangoPrecioParametrizado :: Int -> (Int, Int) -> String
rangoPrecioParametrizado a (b,c) |a < 0 = "Esto no puede ser" 
                                 |(a >= b) && (a <= c) = "Hay que verlo bien"
                                 |a < b = "Muy barato"
                                 |a > c = "Demasiado caro"


mayor3 :: (Int,Int,Int) -> (Bool,Bool,Bool)
mayor3 (a,b,c) = (a>3,b>3,c>3) 


todosIguales :: (Int,Int,Int) -> Bool
todosIguales (a,b,c) = a == b && b == c 

soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) | (mod x 2) == 0 = x : (soloPares xs)
                 | otherwise = soloPares xs


mayoresQue :: Int -> [Int] -> [Int]
mayoresQue a [] = []
mayoresQue a (x:xs) | (x > a) = x : ( mayoresQue a xs)
                    | otherwise = mayoresQue a xs



todosMenores10 :: [Int] -> Bool
todosMenores10 [] = True
todosMenores10 (x:xs) = (x > 10) && todosMenores10 xs 


hay0 :: [Int] -> Bool
hay0 [] = False
hay0 (x:xs) = (x == 0) || hay0 xs


repartir :: [String] -> [String] -> [(String,String)]
repartir xs [] = []
repartir [] ys = []
repartir (x:xs) (y:ys) = (x,y) : repartir xs ys 


apellidos :: [(String,String,Int)] -> [String]
apellidos [] = []
apellidos ((a,b,c):xs) = b : (apellidos xs) 