import System.Directory.Internal.Prelude (toLower)
-- William Andrade Da Silva
-- 23/OUT/2021



type Tomador = [Char ]
type Livro = [Char]
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


-- A função ignore case, compara string ignorando se são maiusculas ou minisculas 
equalsIgnoreCase :: [Char] -> [Char] -> Bool
equalsIgnoreCase x y = map toLower x == map toLower y


-- Retorna uma lista de livros pegado por x pessoa
getLivroEmprestadoByPessoa :: Tomador -> [Livro]
getLivroEmprestadoByPessoa y  = map fst (filter (\(x,z) -> equalsIgnoreCase z y  ) dblivrosEmprestados )

-- Retorna uma lista de pessoas que pegaram x livro
getTomadorByLivro :: Livro -> [Tomador]
getTomadorByLivro livro = map snd (filter (\(x,z) -> equalsIgnoreCase livro x  ) dblivrosEmprestados )

-- Retorna True para livro emprestado e false para livro nao emprestado
isEmprestado :: Livro -> Bool
isEmprestado livro =  length (filter (\(x,z) -> equalsIgnoreCase livro x  ) dblivrosEmprestados) > 0 


-- conta quantos livros x pessoa pegou 
countLivrosEmprestados ::   Tomador -> Int 
countLivrosEmprestados pessoa =  length (filter (\(x,z) -> equalsIgnoreCase pessoa z  ) dblivrosEmprestados)

