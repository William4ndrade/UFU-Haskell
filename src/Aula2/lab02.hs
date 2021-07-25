inc :: Int -> Int
inc x = x + 1
quadrado :: Int -> Int
quadrado x = x * x
media :: Float -> Float -> Float
media a b = ( a + b) / 2.0


--inc (quadrado 5) R=26
--quadrado (inc 5) R=36
--media (inc 3)(inc 5) R= Erro, Espera float e é passado int




-- Quarta potencia
qp :: Int -> Int
qp x = quadrado (quadrado x)

------------------------------

-- Segundos para horas 
qhoras :: Float -> Float   
qhoras x =  (x/60) / 60
----------------------------------

-- Segundos para minutos
qminutos :: Float  -> Float  
qminutos sec =  qhoras sec  * 60

-----------------------------------------

-- Valores Logicos

logica :: Bool -> Bool -> Bool
logica x y = (x || y) && not (x && y)

-----------------------------------------------------




