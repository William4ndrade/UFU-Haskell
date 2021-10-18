import Data.Char



upper :: String -> String 
upper [] = ""
upper (x:xs) = toUpper x:(upper xs)


upper2 :: String -> String 
upper2 [] = ""
upper2 (x:xs)
        | isAlpha x =  toUpper x:(upper2 xs)
        | otherwise = upper2 xs