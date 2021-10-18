termo_n :: Int -> Int
termo_n 1 = 0
termo_n 2 = 1
termo_n x = termo_n (x-1) + termo_n (x-2) 
        