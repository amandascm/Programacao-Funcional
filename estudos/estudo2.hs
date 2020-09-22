--listas sao estruturas recursivas
import Data.Char

--somar os elementos de uma lista de inteiros
sumList :: [Int] -> Int
sumList list
    | list == [] = 0
    | otherwise = head list + sumList (tail list)

--dobrar os elementos de uma lista
doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList (h:t) = [2*h]++doubleList t

--verificar se um elemento esta na lista
--membership :: [Int] -> Int -> Bool
membership [] x = False
membership (h:t) x
    | h == x = True
    | otherwise = membership t x

--filtrar apenas os numeros de uma string
digitsFromList :: [Char] -> [Char]
digitsFromList [] = []
digitsFromList str = [ch | ch <- str, membership ['0'..'9'] ch]

--somar os elementos de duas listas
sumLists :: [Int] -> [Int] -> [Int]
sumLists [] [] = []
sumLists l1 l2 = [(head l1)+(head l2)] ++ sumLists (tail l1) (tail l2)

--quicksort em haskell
menorQuePivo :: [Int] -> Int -> [Int]
menorQuePivo list p = [x | x <- list, x<p]

maiorQuePivo :: [Int] -> Int -> [Int]
maiorQuePivo list p = [x | x <- list, x>=p]

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (h:t) = quicksort (menorQuePivo t h) ++ [h] ++ quicksort (maiorQuePivo t h)

--dado um inteiro N, obter lista com N primeiros inteiros da seq de fibonacci
fibonacci :: Int -> Int
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2)

fibo :: Int -> [Int]
fibo n = [fibonacci x | x <- [1..n]]

type Name = String
type Age = Int
type Phone = Int
type Person = (Name, Age, Phone)

getPhone :: Person -> Phone
getPhone (n, a, p) = p

--resolucao de funcao quadratica: encontrar raizes

umaRaiz :: Float -> Float -> Float -> Float
umaRaiz a b c = -b/2*a

duasRaizes :: Float -> Float -> Float -> (Float, Float)
duasRaizes a b c = (w, z)
    where   w = -b + sqrt(b^2 - 4*a*c)
            z = -b - sqrt(b^2 - 4*a*c)

raizes :: Float -> Float -> Float -> [Float]
raizes a b c
    | b^2 < 4*a*c = []
    | b^2 > 4*a*c = [fst (duasRaizes a b c), snd (duasRaizes a b c)]
    | otherwise = [umaRaiz a b c]

--funcao que recebe tres inteiros e retorna uma tupla com o menor e o maior deles
menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior x y z = (head list, last list)
    where   list = quicksort [x,y,z]

--funcao que recebe uma tripla e retorna a tripla ordenada
ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (x, y, z) = (list!!0, list!!1, list!!2)
    where list = quicksort [x,y,z]

type Ponto = (Float, Float)
--retornar a primeira coordenada de um ponto
retornaX :: Ponto -> Float
retornaX p = fst p
--retornar a segunda coordenada de um ponto
retornaY :: Ponto -> Float
retornaY p = snd p

type Reta = (Ponto, Ponto)
--retornar se a reta eh vertical
ehVertical :: Reta -> Bool
ehVertical (p1,p2) = fst p1 == fst p2
--retornar ordenada do ponto com abscissa x que faça parte da reta R
pontoY :: Float -> Reta -> Float
pontoY x (p1,p2) = ((snd p2 - snd p1)/(fst p2 - fst p1))*(x - fst p1) + snd p1

--exemplo biblioteca
type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

--livros emprestados
baseExemplo :: BancoDados
baseExemplo = [("Amanda", "Lavoura arcaica"),
                ("Tales", "O livro dos abracos"),
                ("Alice", "Memoria postumas de bras cubas"),
                ("Tales", "A metamorfose"),
                ("Amanda", "O livro dos abracos")]

--livros emprestados por uma pessoa
livros :: BancoDados -> Pessoa -> [Livro]
livros banco pessoa = [l | (p, l)<-banco, p == pessoa]

--emprestimos de um livro
emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos banco livro = [p | (p, l)<-banco, l == livro]

--se o livro esta emprestado ou nao
emprestado :: BancoDados -> Livro -> Bool
emprestado banco livro = length [p | (p, l)<-banco, l == livro] > 0

--quantidade de emprestimos feitos por uma pessoa
qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos banco pessoa = length (livros banco pessoa)

--funcao emprestar livro
emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar banco pessoa livro = banco ++ [(pessoa, livro)]

--funcao devolver
devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver banco pessoa livro = [(p,l) | (p,l)<-banco, p/=pessoa || l/=livro]

--funcao que diz de um inteiro eh membro da lista
--membro :: [Int] -> Int -> Bool
membro lista x = length [1 | y<-lista, y==x] > 0

--quicksort com compreensao de lista
quicksortComp :: [Int] -> [Int]
quicksortComp [] = []
quicksortComp (h:t) = quicksortComp [x | x<-t, x<h] ++ [h] ++ quicksortComp [x | x<-t, x>=h]

--Processamento de texto

--funcao que retorna primeira palavra da string (ate o primeiro espaco)
getWord :: String -> String
getWord [] = []
getWord (h:t)
    | h /= ' ' = [h] ++ getWord t
    | otherwise = []

--funcao que retorna string sem elementos antes da primeira sequencia de espacos
dropWord :: String -> String
dropWord [] = []
dropWord (h:t)
    | h /= ' ' = dropWord t
    | otherwise = (h:t)

--funcao que retorna string sem primeira sequencia de espacos
dropSpace :: String -> String
dropSpace [] = []
dropSpace (h:t)
    | h == ' ' = dropSpace t
    | otherwise = (h:t)

--funcao que retorna lista de sequencias de letras em uma string
splitWords :: String -> [String]
splitWords [] = []
splitWords (h:t)
    | h == ' ' = splitWords (dropSpace (h:t))
    | otherwise = [getWord (h:t)] ++ splitWords (dropWord (h:t))

type Wordd = String
type Line = [Wordd]

--funcao para pegar n palavras de uma linha
getLinee :: Int -> Line -> Line
getLinee 0 list = []
getLinee n (h:t) = [h] ++ (getLinee (n-1) t)

btoi :: String -> Int
btoi [] = 0
btoi (h:t) = (Data.Char.digitToInt h)*2^((length (h:t)) - 1) + btoi t
