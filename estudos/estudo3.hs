
--funcao de alta ordem que recebe uma funcao (de dois parametros) como um dos parametros e aplica aos outros dois
applyBinOper :: (t->t->a)->t->t->a
applyBinOper f x y = f x y

{-funcoes de mapeamento: recebem uma lista,
transformam seus elementos e retornam a lista
com os elementos processados. Ex.:-}

doubleListElements :: [Int] -> [Int]
doubleListElements (h:t) = 2*h : (doubleListElements t)

{-funcao para alterar todos os elementos de uma 
lista aplicando a eles uma funcao (ja existe em Prelude.map-}
mapp :: (t -> u) -> [t] -> [u]
{-assinatura significa:
primeiro parametro: uma funcao que recebe um parametro do tipo t e retorna um valor do tipo u
segundo parametro: uma lista de elementos do tipo t
terceiro parametro: uma lista de elementos do tipo u-}
mapp f [] = []
mapp f (h:t) = f h : mapp f t

mappp f list = [f l | l <- list]

{-funcoes de reducao ou dobramento: 
funcoes que reduzem uma estrutura de dados a
uma estrutura mais simples (combinam os elementos). Ex.:-}
concatList :: [String] -> String
concatList [] = []
concatList (h:t) = h ++ concatList t

--funcao FOLD
foldd :: (t -> t -> t) -> [t] -> t
foldd f [h] = h
foldd f (h:t) = f h (foldd f t)

--usando fold
sumList :: [Int] -> Int
sumList [] = 0
sumList list = foldd (+) list

andList :: [Bool] -> Bool
andList list = foldd (&&) list

--foldr: funcao implementada por haskell
--parametros: funcao, caso base para uma lista vazia, lista
foldrr :: (t -> u -> u) -> u -> [t] -> u
foldrr f casobase [] = casobase
foldrr f casobase (h:t) = f h (foldrr f casobase t)

--usando foldr
concList :: [String] -> String
concList list = foldrr (++) [] list

{-funcoes de filtragem-}

--filter (funcao ja implementada em haskell)
--toda funcao que recebe um parametro e retorna um Bool pode ser chamada de predicado
filt :: (t -> Bool) -> [t] -> [t]
filt f [] = []
filt f (h:t)
    | f h = h : filt f t
    | otherwise = filt f t

--com comp de listas
filtt p l = [a | a <- l, p a]

--usando filt

listPares l = filt isPar l
    where isPar x = (mod x 2 == 0)

--verificar se uma funcao e crescente no intervalo de 0 a n
isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f 0 = True
isCrescent f n
    | f n >= f (n-1) = isCrescent f (n-1)
    | otherwise = False

db x = -2*x

--funcao acima com foldr
isCres func n = foldr (\x prev -> (func x >= func (x-1)) && prev) True [0..n]

--funcao acima com compreensao de listas
isCresc func n = length [func i | i <- [0..(n-1)], func i > func (i+1)] == 0

--retorna lista com elementos elevados ao quadrado (funcao de transformacao)
aoQuadrado l = map (^2) l

--retorna soma dos quadrados dos elementos da lista (funcao de combinacao)
somaLista l = foldr (+) 0 (map (^2) l)

--retorna lista com os elementos maiores que zero
maioresQueZero l = filter (>0) l

--funcao map implementada com foldr
mapComFoldr func list = foldr (\x y -> func x:y) [] list

{-funcao que recebe uma lista de listas de inteiros e devolve 
uma lista com os maiores elementos de cada sublista -}
--foldr interno: retorna o elemento maximo de uma lista
--foldr externo: concatena o elemento maximo da sublista anterior com o maior elemento da sublista atual
maiores:: [[Int]] -> [Int]
maiores list = foldr (\sublist prevSubList -> (foldr (\x y -> max x y) 0 sublist):prevSubList) [] list


{-funcao take (ja implementada em haskell) ate o elemento ser igual ao espaco - sem pegar o espaco-}
--primeiro usa foldl para formar a string com os caracteres do comeco da string ate o primeiro espaco
--depois usa o filter para eliminar o espaco
takeWhilee pred str = filter pred (foldl (\x y -> func x [y]) [] str)
    where func x y
            | length x >= 1 && not (pred (last x)) = x
            | otherwise = x++y

{-funcao dropWhile (ja implementada em haskell): pega os elementos ate que o primeiro deles nao atenda ao predicado-}
--dropWhilee :: Ord t => (t -> Bool) -> [t]
dropWhilee pred list = (foldl (\a b -> func a b) [] list)
    where func a b
            | (length a >= 1) && not (pred (head a)) = a++[b]
            | (length a == 0) && not (pred b) = a++[b]
            | otherwise = a

getWord :: String -> String
getWord str = takeWhilee (/=' ') str

dropWord :: String -> String
dropWord str = dropWhilee (/=' ') str

dropSpace :: String -> String
dropSpace str = dropWhilee (==' ') str

--exercicio slide funcoes como valores

f2:: (t -> u -> v) -> (u -> t -> v)
f2 f = (\x y -> f y x)