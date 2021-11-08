import GHC.Unicode (toLower)

-- William Andrade Da Silva
-- 6/out/2021


type Tomador = String
type Livro = String
type Database = [(Tomador, Livro)]



dblivrosEmprestados :: Database
dblivrosEmprestados = [
            ("Sapiens", "William"),
            ("Harry potter e a pedra filosofal", "Bianka"),
            ("A Menina que Roubava Livros", "Victoria"),
            ("Harry potter reliquias da morte", "Bianka"),
            ("Sapiens", "Jesus"),
            ("algum harry potter dnv", "william")
        ]




equalsIgnoreCase :: [Char] -> [Char] -> Bool
equalsIgnoreCase x y = map toLower x == map toLower y

getLivroEmprestadoByPessoa :: Tomador -> Database -> [Livro]
getLivroEmprestadoByPessoa y db = [ fst x | x <- db,  equalsIgnoreCase (snd x) y]


getLivroEmprestadoByPessoaRecursivo :: Tomador -> Database -> [Livro]
getLivroEmprestadoByPessoaRecursivo _ [] = []
getLivroEmprestadoByPessoaRecursivo nome db 
                                        | equalsIgnoreCase nome (snd (head db)) = fst  (head db): getLivroEmprestadoByPessoaRecursivo nome (tail db)
                                        | otherwise = getLivroEmprestadoByPessoaRecursivo nome (tail db)



getTomadorByLivro :: Livro -> Database -> [Tomador]
getTomadorByLivro livro db = [ snd x | x <- db, equalsIgnoreCase (fst x) livro]


getTomadorByLivroRecursivo :: Livro -> Database -> [Tomador]
getTomadorByLivroRecursivo _ [] = []
getTomadorByLivroRecursivo livro db 
                            | equalsIgnoreCase livro (fst (head db)) =  snd (head db) :  getTomadorByLivroRecursivo livro (tail db)
                            | otherwise = getTomadorByLivroRecursivo livro (tail db)

isEmprestado :: Livro -> Database -> Bool 
isEmprestado livro db = length [ x | x <- db, equalsIgnoreCase livro (fst x)] > 0


isEmprestadoRecursivo :: Livro -> Database -> Bool 
isEmprestadoRecursivo _ [] = False 
isEmprestadoRecursivo livro db 
                        | equalsIgnoreCase livro  (fst (head db)) = True 
                        | otherwise = isEmprestadoRecursivo livro (tail db)

countLivrosEmprestados :: Tomador -> Database -> Int 
countLivrosEmprestados pessoa db = length [x | x <- db, equalsIgnoreCase pessoa (snd x) ]


countLivrosEmprestadosRecursivo :: Tomador -> Database -> Int 
countLivrosEmprestadosRecursivo _ [] = 0
countLivrosEmprestadosRecursivo pessoa db 
                                | equalsIgnoreCase pessoa (snd (head db)) = 1 + countLivrosEmprestadosRecursivo pessoa (tail db)
                                | otherwise = countLivrosEmprestadosRecursivo pessoa (tail db)