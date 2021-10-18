l1 = [1,2,3]
a = [1 + n | n <- l1]




dobrapos :: [Int] -> [Int]
dobrapos [] = []
dobrapos (x:xs) 
        | x > 0 = (x*2):(dobrapos xs)
        | otherwise = dobrapos xs


dobrapossemrec xs = [x*2 | x <- xs, x > 0]




