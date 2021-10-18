-- Nome: William Andrade Da Silva
-- Data: 1/out/2021


-- Produção de Um Recibo Fiscal em um Supermercado

type Nome = String
type Preco = Int
type CodBar = Int
type BaseDeDados = [(CodBar,String,Preco)]
type ListaDeCodigos = [CodBar]
type Recibo = [(String,Preco)]



listaDeProdutos :: BaseDeDados
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


formataCentavos :: Preco -> String
formataCentavos x = show inteiros ++ "." ++ show centavos
            where
                inteiros = div x 100
                centavos =  rem x 100
        



tamLinha :: Int
tamLinha = 30

gerarpontinhos :: Int -> String
gerarpontinhos uso = replicate (tamLinha - uso ) '.'

formataLinha :: (String,Preco) -> String 
formataLinha (n,p) = n ++ gerarpontinhos (lennome + lenpreco) ++ formataCentavos p ++ "\n"
            where
                lennome = length n
                lenpreco = length (formataCentavos p)
            

    

geraTotal :: Recibo -> Preco
geraTotal [] = 0
geraTotal ((n,p):xs) = p + geraTotal xs


formataTotal :: Preco -> String
formataTotal x = "Total" ++ gerarpontinhos (outroschar + lenpreco)  ++ "$" ++ formataCentavos x ++ "\n"
            where
                lenpreco = length (formataCentavos x)
                outroschar = 6

        
acha :: BaseDeDados -> CodBar -> (String,Preco)
acha [] _ = ("item desconhecido", 0)
acha ( (c, n, p):xs) num 
                    | num == c = (n, p)
                    | otherwise = acha xs num

achaItem :: CodBar -> (String,Preco)
achaItem x = acha listaDeProdutos x



fazRecibo :: ListaDeCodigos -> Recibo
fazRecibo [] = []
fazRecibo (x:xs) =   (achaItem x):fazRecibo xs


formataRecibo :: Recibo -> String
formataRecibo [] =""
formataRecibo (x:xs) = supermercadoname ++ linhas (x:xs) ++ total
                where 
                    total = formataTotal (geraTotal (x:xs))
                    linhas x 
                            | length x == 0 = ""
                            | otherwise  = formataLinha (head x) ++ linhas (tail x)
                    supermercadoname = "Supermercado QLegal\n"
            
              
    
    
geraRecibo :: ListaDeCodigos -> String
geraRecibo lc = formataRecibo( fazRecibo lc)



main :: IO ()
main = putStr (geraRecibo [1234, 3216, 4719,1112,1113,3814])