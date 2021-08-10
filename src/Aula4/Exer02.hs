-- William Andrade Da Silva



-- 1
terceiro :: [Integer] -> Integer  
terceiro x = head (tail (tail x))

-----------------------------------------

-- 2 - 
-- A) -- Ultimo char de uma string

ultimo ::String->Char 
ultimo x = head (reverse x)
--ultimo x = last x -- Outra versao

--------------------------------------------------
-- B) -- String sem o ultimo char

nultimo :: String -> String 
nultimo x = reverse (tail (reverse x))
-- nultimo x = init x -- Outra versao


---------------------------------------------------


-- 3 - Volta as iniciais 

ini :: String -> String -> (Char, Char)
ini x y = (head x, head y)

