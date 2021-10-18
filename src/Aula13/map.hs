

f :: [t] -> (t -> a) -> [a]
f [] g = []
f (x:xs) g = (g x): (f xs g)



primeiros :: [(a, b)] -> [a]
primeiros [] = []
primeiros (x:xs) = (fst x):(primeiros xs)

first :: [(a, b)] -> [a]
first x = map fst x


ellen :: Foldable t => [t a] -> [Int]
ellen  = map length 