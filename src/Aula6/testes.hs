
calcula:: Char -> Int -> Int -> Int
calcula cal x y
 | (cal == '*') = (x * y)
 | (cal == '/') = (div x y)
 | otherwise = error "Exception: erro!"