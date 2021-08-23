mdc :: Int -> Int -> Int 
mdc x y 
    | y == 0 = x
    | y > 0 = mdc y (mod x y)
    | otherwise = error "Valor de Y negativo"


mdc3 :: Int -> Int -> Int -> Int 
mdc3 a b c = mdc (mdc a b) c


somar n 
    | n == 1 = 1
    | otherwise =  somar (n-1) + n

somaentre :: Int -> Int -> Int 
somaentre a b 
        | a == b = a
        | otherwise = (somaentre a (b-1) ) + b
        
        