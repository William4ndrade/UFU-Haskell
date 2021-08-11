
notlogico :: Bool -> Bool 

notlogico x
    | x = False 
    | otherwise  = True 


elogico :: Bool -> Bool -> Bool 
elogico x y
        | x && y = True 
        | otherwise = False 

oulogico :: Bool -> Bool -> Bool 
oulogico x y 
        | x || y = True 
        | otherwise = False 