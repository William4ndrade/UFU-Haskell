
fatorial n  
    | n == 0 = 1
    | otherwise  =  n * fatorial (n-1)
    
   
somar n 
    | n == 1 = 1
    | otherwise =  somar (n-1) + n