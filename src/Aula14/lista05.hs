import Text.Show (Show)
-- William Andrade Da Silva
-- 18/out/2021


-- Produção de Um Recibo Fiscal em um Supermercado





listaDeProdutos :: Integral a => [(a,[Char],a)]
listaDeProdutos = [ (1234, "Oleo DoBom, 1l" , 195),
                (4756, "Chocolate Cazzeiro, 250g" , 180),
                (3216, "Arroz DoBom, 5Kg", 213),
                (5823, "Balas Pedregulho, 1Kg" , 379), 
                (4719, "Queijo Mineirim, 1Kg" , 449),
                (6832, "Iogurte Maravilha, 1Kg" , 499),
                (1112, "Rapadura QuebraDente, 1Kg", 80),
                (1111, "Sal Donorte, 1Kg", 221),
                (1113, "Cafe DoBom, 1Kg", 285),
                (1115, "Biscoito Bibi, 1Kg", 80),
                (3814, "Sorvete QGelo, 1l", 695)] 



formataCentavos :: (Show a,Integral a) => a -> [Char]
formataCentavos x = show inteiros ++ "." ++ show centavos
            where
                inteiros = div x 100
                centavos =  rem x 100
        



tamLinha :: Integral a => a
tamLinha = 30


gerarpontinhos :: Int -> [Char]
gerarpontinhos uso = replicate (tamLinha - uso ) '.'


formataLinha :: (Show a, Integral a) => ([Char], a) -> [Char]
formataLinha (n,p) = n ++ gerarpontinhos (lennome + lenpreco) ++ formataCentavos p ++ "\n"
            where
                lennome = length n
                lenpreco = length (formataCentavos p)
            

    


geraTotal :: Num p => [(a, p)] -> p
geraTotal [] = 0
geraTotal ((n,p):xs) = p + geraTotal xs



formataTotal :: (Show a, Integral a) => a -> [Char]
formataTotal x = "Total" ++ gerarpontinhos (outroschar + lenpreco)  ++ "$" ++ formataCentavos x ++ "\n"
            where
                lenpreco = length (formataCentavos x)
                outroschar = 6

        

acha :: (Num b, Eq t) => [(t, [Char], b)] -> t -> ([Char], b)
acha [] _ = ("item desconhecido", 0)
acha ( (c, n, p):xs) num 
                    | num == c = (n, p)
                    | otherwise = acha xs num


achaItem :: Integral b => b -> ([Char], b)
achaItem x = acha listaDeProdutos x




fazRecibo :: Integral b => [b] -> [([Char], b)]
fazRecibo [] = []
fazRecibo (x:xs) =   (achaItem x):fazRecibo xs



formataRecibo :: (Show a, Integral a) => [([Char], a)] -> [Char]
formataRecibo [] =""
formataRecibo (x:xs) = supermercadoname ++ linhas (x:xs) ++ total
                where 
                    total = formataTotal (geraTotal (x:xs))
                    linhas x 
                            | length x == 0 = ""
                            | otherwise  = formataLinha (head x) ++ linhas (tail x)
                    supermercadoname = "Supermercado QLegal\n"
            
              
    
    

geraRecibo :: (Show a, Integral a) => [a] -> [Char]
geraRecibo lc = formataRecibo( fazRecibo lc)




main = putStr (geraRecibo [1234, 3216, 4719,1112,1113,3814])