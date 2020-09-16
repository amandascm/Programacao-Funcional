--equacoes de segundo grau

raizes :: (Float, Float, Float) -> [Float]
raizes (0,0,0) = [-99999]
raizes (a,b,c)
	| b*b > 4*a*c = [( ((-1)*b + sqrt(b*b - 4*a*c))/(2*a) ), ( ((-1)*b - sqrt(b*b - 4*a*c))/(2*a) ) ]
	| b*b == 4*a*c = [((-1)*b)/(2*a)]
	| otherwise = [-88888]


--compreensao de listas / banco de dados

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

baseEx :: BancoDados
baseEx = [("Amanda", "Lavoura Arcaica"),
	("Tales", "A Metamorfose"),
	("Ana Luíza", "Felicidade Clandestina"),
	("Amanda", "A Desumanizacao")]

ismember :: Int -> [Int] -> [Int]
ismember x lista = [a | a <- lista, x == a]

--membro :: Int -> [Int] -> Bool
membro x lista
	| (ismember x lista) == [] = False
	| otherwise = True

livros :: BancoDados -> Pessoa -> [Livro]
livros banco pessoa = [ l | (p, l) <- banco, p == pessoa]

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos banco livro = [ p | (p,l) <- banco, l == livro]

emprestado :: BancoDados -> Livro -> Bool
emprestado [] livro = False
emprestado banco livro
	| livro == (snd (head banco)) = True
	| otherwise = emprestado (tail banco) livro

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos banco pessoa = length (livros banco pessoa)

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver banco pessoa livro = [ (p,l) | (p,l) <- banco, (p /= pessoa || l /= livro)]

