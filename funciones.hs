triple :: Int -> Int
triple x = x *3



signo :: Int -> Int
signo x |x > 0 = 1
        |x < 0 = -1
        |x == 0 = 0


        
valAbs :: Int -> Int 
valAbs x|x >= 0 = x
        |x < 0 = -x



edad :: (String,Int) -> Int
edad (nom,ed) = ed



notaFinal :: (String,Float,Float,Float) -> Float
notaFinal (nombre,n1,n2,n3) = (n1+n2+n3)/3 


  
condicion :: (String, Float,Float,Float) ->String
condicion e |notaFinal e >= 7 = "rinde coloquio"
            |otherwise = "regular" 


notaycon :: (String,Float,Float,Float) -> (Float,String)
notaycon a = (notaFinal a, condicion a)


duplicaS :: Int ->Int
duplicaS a = 2*a


esBisiesto :: Int -> Bool
esBisiesto año = ((mod año 400) == 0) || ((mod año 4) == 0 && (mod año 100) /= 0)


max3 :: Int ->Int -> Int ->Int
max3 x y z = max (max x y) z


min3 :: Int ->Int -> Int ->Int
min3 x y z = min (min x y) z

dispersion :: Int -> Int -> Int -> Int
dispersion x y z = (max3 x y z) - (min3 x y z)

mayor3 :: (Int,Int,Int) -> (Bool,Bool,Bool)
mayor3 (a,b,c) = (a>3,b>3,c>3)

segundo3 :: (Int,Int,Int) -> Int
segundo3 (a,b,c) = b

ordena :: (Int,Int) -> (Int, Int)
ordena (a,b) |a>b = (b,a)
             |a<b = (a,b)   

ordena3 :: (Int,Int,Int) -> (Int,Int,Int)
ordena3 (a,b,c) |a<b && b<c = (a,b,c)
                |b<a && a<c = (b,a,c)
                |b<c && c<a = (b,c,a)
                |c<a && a<b = (c,a,b)
                |a<c && c<b = (a,c,b)
                |c<b && b<a = (c,b,a)

                                --- Recursión --- 

contar :: [Int] -> Int 
contar [] = 0
contar (x:xs) = 1 + contar(xs)                                

lista = [1,2,3,4,5,6,7]


sumaL :: [Int] -> Int 
sumaL [] = 0
sumaL (x:xs) = x + sumaL xs 

productoL :: [Int] -> Int 
productoL [] = 1
productoL (x:xs) = x * productoL xs 


card :: [Int] -> Int
card [] = 0
card (x:xs) = 1 + card xs 


todosMenores10 :: [Int] -> Bool
todosMenores10 [] = True
todosMenores10 (x:xs) = x < 10 && todosMenores10 xs 


hay0 :: [Int] -> Bool
hay0 [] = False 
hay0 (x:xs) = x == 0 || hay0 xs 


sumar1 :: [Int] -> [Int]
sumar1 [] = []
sumar1 (x:xs) = (x + 1) : sumar1 xs 


duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = (x*2) : duplica xs 


multiplica :: Int -> [Int] -> [Int]
multiplica n [] = []
multiplica n (x:xs) = (n*x) : multiplica n xs 


soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs)| mod x 2 == 0 = x : soloPares xs 
                |otherwise = soloPares xs


mayoresQue10 :: [Int] -> [Int]
mayoresQue10 [] = []
mayoresQue10 (x:xs) |x > 10 = x : mayoresQue10 xs 
                    |otherwise = mayoresQue10 xs 


mayoresQue :: Int -> [Int] -> [Int]                    
mayoresQue n [] = []
mayoresQue n (x:xs) |x > n = x : mayoresQue n xs 
                    |otherwise = mayoresQue n xs 


-- toma una lista xs (a izquierda) y un natural n que indica una cantidad, y devuelve la sublista con los primeros n elementos de xs. Ej: 
-- [1, 2, 3, 4, 5, 6] ↑ 2 = [1, 2]

tomar :: Int -> [Int] -> [Int]
tomar n _|n <= 0 = []
tomar _ [] = [] 
tomar n (x:xs) = x : (tomar (n-1) xs) 

