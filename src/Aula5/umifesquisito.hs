
m :: Int -> Int -> Int 
m x y 
    | x <= y = x 
    | otherwise  = y


fmenor :: (Int ,Int,Int) -> Int 
fmenor (x,y,z) 
    | x == y && x == z = 3
    | x == y || x == z || y == z = 2
    | otherwise = 0 


fmenor3 :: (Int, Int ,Int) -> Int 
fmenor3 (x, y,z) 
    | x <= y && x <= z = x 
    | y <= x && y <= z = y 
    | otherwise  = z 





fmaioremenor :: (Int,Int,Int) -> (Int,Int)
fmaioremenor (x, y,z ) 
    | x >= y && x >= z = (x, fmenor3 (x,y,z))
    | y >= z = (y, fmenor3 (x,y,z))
    | otherwise = (z, fmenor3 (x,y,z))



