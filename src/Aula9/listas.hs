mytail (x:xs) = xs

myhead (x:xs) = x

mylen :: [Int] -> Int 
mylen [] = 0
mylen (x:xs) =  1 + (mylen xs)
    

myreverse :: [Int] -> [Int]
myreverse [] = []
myreverse (x:xs) = (myreverse xs) ++ [x]


somaimpares :: [Int] -> Int 
somaimpares [] = 0
somaimpares (x:xs) = somaimpares xs + value
            where
                value = if (mod x 2) /= 0 then x else 0



somaquadrados :: [Int] -> Int 
somaquadrados [x] = (x*x)
somaquadrados (x:xs) =  somaquadrados xs + (x*x) 


maior :: [Int] -> Int
maior [x] = x 
maior (x:xs) 
        | x >= (maior xs) = x
        | otherwise = maior xs



desduplicar :: [Int] -> [Int]
desduplicar [] = []
desduplicar (x:xs) = x:(desduplicar xs)
            