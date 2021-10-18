import Distribution.Simple.Utils (xargs)


n_esimo :: Int -> [Int] -> Int 
n_esimo n l 
        | n == 0 = head l
        | otherwise = n_esimo (n-1) (tail l)
        


duplica :: [Int] -> [Int]
duplica [x] = [x,x]
duplica (x:xs) =  x:x:(duplica xs)

my_reverse :: [Int] -> [Int]
my_reverse [] = []
my_reverse (x:xs) = (my_reverse xs) ++ [x]


replace_list :: Int -> Int -> [Int] -> [Int ]
replace_list _ _ [] = []
replace_list td n (x:xs) 
                    | x==td = n:(replace_list td n xs)
                    | otherwise  = x:(replace_list td n xs)


maior :: [Int] -> Int
maior [x] = x 
maior (x:xs) 
        | x >= (maior xs) = x
        | otherwise = maior xs



desduplicar :: [Int] -> [Int]
desduplicar [x] = []
desduplicar (x:xs)
        | x == (head xs) =  x:(desduplicar xs)
        | otherwise = desduplicar xs


-- versao professor -----------------------> 

desduplica :: [Int] -> [Int]
desduplica [] = []
desduplica (x:y:xs) = y:(desduplica xs)

-------------------------------------------


insere :: Int -> [Int] -> [Int]
insere n [] = [n]
insere n (x:xs) 
                | n <= x =  n:x:xs
                | otherwise = x:(insere n xs)


ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar (x:xs) = insere x (ordenar xs)
     
      

pertence :: Int -> [Int] -> Bool 
pertence n [] = False 
pertence n (x:xs) 
        | x == n = True 
        | otherwise = (pertence n xs)


cocatenar :: [[Int]] -> [Int ]
cocatenar [] = []
cocatenar (x:xs) =  x ++ (cocatenar xs) 




uniao :: [Int] -> [Int] -> [Int]
uniao [] (x:xs) = (x:xs)
uniao (x:xs) (y:ys) 
        | not (pertence x (y:ys)) = x:(uniao xs (y:ys))
        | otherwise = uniao xs (y:ys)  

