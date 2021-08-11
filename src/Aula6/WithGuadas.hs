
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


ou_logico :: (Bool, Bool) -> Bool
ou_logico (False, False) = False 
ou_logico (_,_)          = True