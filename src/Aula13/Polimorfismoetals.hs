segundo :: [a] -> a
segundo xs = head ( tail xs)


trocar:: (a,b) -> (b,a)
trocar (x, y) = (y, x)

parear :: a -> b -> (a,b)
parear x y = (x, y)

dobro :: Num a => a -> a
dobro x = x * 2

palindromo :: Eq a => [a] -> Bool
palindromo xs = reverse xs == xs


terceiro :: (a, b, c) -> c
terceiro (x,y,z) = z

equals :: (Eq a, Integral b) => a -> a -> a -> b
equals x y z 
        | x == y && x == z = 3
        | x == y || x == z || y == z = 2
        | otherwise = 0


maiormenortupla :: (Ord b) => (b,b,b) -> (b,b)
maiormenortupla (x,y,z) = (maior, menor) 
                where 
                    maior 
                        | x > y && x > z = x
                        | y > x && y > z = y
                        | otherwise = z
                    menor 
                        | x < y && x < z = x
                        | y < x && y < z = y
                        | otherwise = z
                        



mylength ::  [a] -> Int 
mylength [] = 0
mylength (x:xs) = (mylength xs) + 1


mysum :: Num a => [a] -> a
mysum [] = 0
mysum (x:xs) = (mysum xs) + x



mysumodd :: Integral p => [p] -> p
mysumodd [] = 0
mysumodd (x:xs)
        | odd x = (mysumodd xs) + x  
        | otherwise = mysumodd xs


mysum3 :: Integral p => [p] -> p
mysum3 [] = 0
mysum3 (x:xs)
        | mod x 3 == 0 = (mysum3 xs) + x  
        | otherwise = mysum3 xs





myprodutoriokkkkkkk :: Num a => [a] -> a
myprodutoriokkkkkkk [] = 1
myprodutoriokkkkkkk (x:xs) = (myprodutoriokkkkkkk xs) * x



pertence :: Eq t => t -> [t] -> Bool
pertence _ [] = False 
pertence y (x:xs) 
            | y == x = True 
            | otherwise = pertence y xs




dobra :: Num a => [a] -> [a]
dobra xs = [ x*2 | x <- xs]

incrementar :: Num a => [a] -> [a]
incrementar xs = [ x+1 | x <- xs]





f :: [t] -> (t -> a) -> [a]
f [] g = []
f (x:xs) g = (g x): (f xs g)

