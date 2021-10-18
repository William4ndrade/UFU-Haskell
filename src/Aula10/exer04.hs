-- William Andrade Da Silva
-- 05/09/21


-- 01

conta_digitos :: Int -> Int 
conta_digitos n 
            | n < 10 && n >= 0 = 1
            | otherwise = conta_digitos (div n 10) + 1



-- 02


soma_digitos :: Int -> Int 
soma_digitos n 
            | n < 10 && n >= 0 = n
            | otherwise = soma_digitos (div n 10) + mod n 10






-- 03
potencia  :: (Int , Int ) -> Int 
potencia (b,e)
        | e == 0 = 1
        | otherwise = potencia(b, e-1) * b
            


-- 04            
ackermann :: (Int,Int) -> Int 
ackermann (m,n)
        | m == 0 = n+1
        | n == 0 = ackermann(m-1, 1) 
        | n > 0 = ackermann(m-1, ackermann(m,n-1))
        | otherwise = -1