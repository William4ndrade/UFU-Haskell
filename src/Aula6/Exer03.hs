
-- William Andrade Da Silva
-- 09/04/21


-- 01 - Corrigida 

saudacaoLegal :: String 
saudacaoLegal = "Ola! Que bom encontrar voce,"
saudacao :: String -> String
saudacao "Joana" = saudacaoLegal ++ "Joana"
saudacao"Fernando"= saudacaoLegal ++ "Fernando"
saudacao nome = saudacaoInfeliz ++ nome
 where
 saudacaoInfeliz= "Nao pensei que ainda estivesse vivo, "


-- 02 - retorna a quantidade de raizes posiveis 

raizes :: Float  -> Float  -> Float  -> Int 
raizes a b c 
    |   x > y = 2
    | x == y = 1
    | otherwise = 0 
    where
        x = b*b
        y = 4 * a * c


-- 03 - Ordena de forma crescente

ordena2 :: Int -> Int -> (Int, Int) 
ordena2 x y 
        | x >= y = (y, x)
        | otherwise = (x,y)



-- 04 - Retorna true se for par e false para impar

par :: Int -> Bool 
par x 
    | mod x 2 == 0 = True 
    | otherwise = False 

-- 05 - Retorna true se for impar e false para par

impar :: Int -> Bool 
impar x 
    | not (par x) = True 
    | otherwise = False 





