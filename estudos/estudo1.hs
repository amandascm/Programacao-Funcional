import Data.Char

--isImpar :: Int -> [Char]
isImpar x = if mod x 2 /= 0
            then "impar"
            else "par"

imc peso h = peso/h**2


isBmenor a b c = if a<b && b<c
                then True
                else False

vendas :: Int -> Int
vendas n = mod n 3

totalVendas :: Int -> Int
totalVendas n
        | (n == 0) = vendas 0
        | otherwise = totalVendas (n-1) + vendas n

semanasComSVendas :: Int -> Int -> Int
semanasComSVendas s n
    | n == 0 = 0
    | (vendas n == s) = 1 + semanasComSVendas s (n-1)
    | otherwise = semanasComSVendas s (n-1)

--determinar se um numero eh primo 
ehPrimo2 :: Int -> Int -> Bool
ehPrimo2 n d
    | d == 0 = False
    | d == 1 = True
    | mod n d == 0 = False
    | otherwise = ehPrimo2 n (d-1)

ehPrimo1 :: Int -> Bool
ehPrimo1 n = ehPrimo2 n (div n 2)

--determinar se dois numeros sao primos entre si
----precisamos saber se o mdc dos numeros eh igual a 1
mdc :: Int -> Int -> Int
mdc x y
    | y == 0 = x
    | otherwise = mdc y (mod x y)

primosEntreSi :: Int -> Int -> Bool
primosEntreSi x y
    | mdc x y == 1 = True
    | otherwise = False

--fatorial
fat :: Int -> Int
fat x
    | x == 0 = 1
    | otherwise = x * fat (x-1)

--compara se quatro números são iguais
all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d
    | a==b && a==c && a==d && b==c && b==d && c==d = True
    |otherwise = False

--retorna quantos parametros sao iguais
--equalCount :: Int -> Int -> Int -> Int

--retorna letra maiuscula
---- chr e ord -> funcoes da bib Data.Char
offset = ord 'A' - ord 'a'
maiuscula letra = chr (ord letra + offset)


--diz se um char eh um digito (0..9)
ehDigito :: Char -> Bool
ehDigito c
    | ord c <= ord '9' && ord c >= ord '0' = True
    | otherwise = False

--definicoes locais (where, let in...)
sumSquares :: Int -> Int -> Int
sumSquares x y = sq x + sq y
    where sq z = z*z


sumSquares2 :: Int -> Int -> Int
sumSquares2 x y = let   sqX = x^2
                        sqY = y^2
                    in sqX + sqY

sumSquares3 :: Int -> Int -> Int
sumSquares3 x y = sqX + sqY
    where   sqX = x*x
            sqY = y*y

--funcao que produz uma string com n espacos
addEspacos :: Int -> String 
addEspacos n
    | n == 0 = []
    |otherwise = " " ++ (addEspacos (n-1))

--funcao que adiciona n espacos a esquerda de uma string
paraDireita :: Int -> String -> String
paraDireita n str = (addEspacos n) ++ str

{-
funcao para retornar em forma de tabela todas as vendas
da semana 0 ate a semana n, incluindo o total e a media de 
vendas no periodo
-}

--retorna lista de strings: cada uma com num. da semana e numero de vendas da semana
semanaVendas :: Int -> [String]
semanaVendas n = [show x ++ "  " ++ show (vendas x) ++ "\n" | x <- [0 .. n]]

--retorna uma strign unindo todos os elementos da lista de strings
semanaVendasStr :: [String] -> String
semanaVendasStr [] = []
semanaVendasStr list = head list ++ (semanaVendasStr (tail list))

--retorna uma string com "Total  " ++ total de vendas da semana 0 a n
totalVendasStr :: Int -> String
totalVendasStr n = "Total" ++ "  " ++ show (totalVendas n) ++ "\n"

--retorna uma string com "Media  " ++ media de vendas da semana 0 a n
mediaVendasStr :: Int -> String
mediaVendasStr n = "Media" ++ "  " ++ show (fromIntegral (totalVendas n)/ fromIntegral n) ++ "\n"

--retorna uma string unica com linhas correspondentes as funcoes acima
tabela :: Int -> String
tabela n = "Semana  Vendas" ++ (semanaVendasStr (semanaVendas n))
            ++ totalVendasStr n ++ mediaVendasStr n

--rever \n