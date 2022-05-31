data Color =  Rojo | Amarillo | Azul | Verde deriving (Show, Eq)
data Forma = Triangulo | Cuadrado | Rombo | Circulo deriving (Show, Eq)
type Figura = (Forma , Color , Int )

rojo :: Figura -> Bool
rojo (for,col,tam) = col == Rojo

azul :: Figura -> Bool
azul (for,col,tam) = col == Azul

verde :: Figura -> Bool
verde (for,col,tam) = col == Verde

amarillo :: Figura -> Bool
amarillo (for,col,tam) = col == Amarillo

tamaño :: Figura -> Int
tamaño (for,col,tam) = tam 

cuadrado :: Figura -> Bool
cuadrado (for,col,tam) = for == Cuadrado

circulo :: Figura -> Bool
circulo (for,col,tam) = for == Circulo

triangulo :: Figura -> Bool
triangulo (for,col,tam) = for == Triangulo

rombo :: Figura -> Bool
rombo (for,col,tam) = for == Rombo

propA :: [Figura] -> Bool
propA [ ] = True
propA (x:xs) = (rojo x) && (propA xs)

propB :: [Figura] -> Bool
propB [] = True
propB (x:xs) = ((tamaño x) < 5) && propB xs

propC :: [Figura] -> Bool
propC [] = True
propC (x:xs) | triangulo x = rojo x && propC xs 
             | otherwise = propC xs 

propD :: [Figura] -> Bool
propD [] = False
propD (x:xs) = (cuadrado x && verde x) || propD xs 

propE :: [Figura] -> Bool 
propE [] = True
propE (x:xs) | circulo x = (azul x && tamaño x < 10) && propE xs
             | otherwise = propE xs                               

propF :: [Figura] -> Bool
propF [] = True 
propF (x:xs) | triangulo x = not (azul x) && propF xs
             | otherwise = propF xs        

propG :: [Figura] -> Bool 
propG [] = True
propG (x:xs) | circulo x = not (amarillo x || verde x) && propG xs 
             | otherwise = propG xs

propH :: [Figura] -> Bool
propH [] = False
propH (x:xs) = (cuadrado x && tamaño x < 5) || propH xs     

exCiR :: [Figura] -> Bool
exCiR [] = False 
exCiR (x:xs) = (circulo x && rojo x) || exCiR xs

exCuR :: [Figura] -> Bool
exCuR [] = False 
exCuR (x:xs) = (cuadrado x && rojo x) || exCuR xs

propI :: [Figura] -> Bool
propI xs = (exCiR xs || exCuR xs) == exCuR xs   
