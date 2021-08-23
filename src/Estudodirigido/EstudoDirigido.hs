type Dia = Int
type Mes = Int
type Ano = Int
type Data = (Dia,Mes,Ano)




bissexto :: Ano  -> Bool 
bissexto x 
    | por4 && not por100 || por400 = True 
    | otherwise = False
    where 
        por4 = mod x 4 == 0
        por100 = mod x 100 == 0
        por400 = mod x 400 == 0


numDeDiasEmCadaMesDeUmAno :: Ano -> [Int]
numDeDiasEmCadaMesDeUmAno x = [31,fev,31,30,31,30,31,31,30,31,30,31]
       where fev
                | bissexto x = 29
                | otherwise = 28


numDeDias :: Data -> Int
numDeDias (dia,mes,ano) =
    dia + sum ((take (mes-1) (numDeDiasEmCadaMesDeUmAno ano))) + (ano-2001)*365 + (ano-2001)`div`4



nomeDoDia :: Int -> String
nomeDoDia x = if x <= 6 then last (take (x+1) days) else error "Apenas valores entre 0 e 6"
        where 
            days = ["Domingo", "Segunda", "Terca", "Quarta", "Quinta", "Sexta", "Sabado"]



diaDaSemana :: Data -> String 
diaDaSemana (dia,mes,ano) = getdayname 0 0
        where
            ndias = numDeDias (dia,mes,ano)
            getdayname :: Int -> Int -> String 
            getdayname  x y
                    | x == ndias = nomeDoDia y
                    | otherwise = getdayname (x+1) (if y+1 > 6 then 0 else y+1)
           
           
            

 
           