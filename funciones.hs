triple :: Int -> Int
triple x = x *3



signo :: Int -> Int
signo x |x > 0 = 1
        |x < 0 = -1
        |otherwise = 0


        
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


duplica :: Int ->Int
duplica a = 2*a


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



