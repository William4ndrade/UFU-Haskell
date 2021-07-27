

modiv:: (Int,Int) -> (Int, Int)
modiv (x,y) =  ((div x y), (mod x y))


segundos :: (Int, Int, Int) -> Int 
segundos (h,m,s) = ((h*60) * 60) + (m*60) + s;


horario :: Int -> (Int, Int, Int)
horario s = (div s 3600, div (mod s 3600) 60, mod (mod s 3600) 60)
            
            
       