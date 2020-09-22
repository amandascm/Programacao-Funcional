import Data.Char


------------ QUESTAO 1

addEspOnStr :: Int -> [Char] -> [Char]
addEspOnStr n [] = []
addEspOnStr n [c] = [c]
addEspOnStr n (h:t)
	| (length((h:t)) - 1) > 0 = [h]++ espacos(n) ++(addEspOnStr n t)
	| otherwise = []

espacos :: Int -> [Char]
espacos n
	| n > 0 = [' '] ++ espacos (n-1)
	| otherwise = []

------------ QUESTAO 2

--Gera uma lista com os algarismos do número
splitNumber :: Int -> [Int]
splitNumber n
	| n >= 10 = splitNumber (div n 10) ++ [mod n 10]
	| otherwise = [n]

--Retorna quantas vezes um interio n aparece numa lista
isMember :: Int -> [Int] -> Int
isMember n [] = 0
isMember n (h:t)
	| n == h = 1 + isMember n t
	| otherwise = isMember n t

--Soma os elementos de uma lista
somaelementos :: [Int] -> Int
somaelementos [] = 0
somaelementos [n] = n
somaelementos (h:t) = h + somaelementos t

--Retorna quantas vezes um dígito d aparece nos algarismos dos inteiros de 0 a n
quantidade :: Int -> Int -> Int
quantidade n d = somaelementos ([(isMember d (splitNumber x)) | x <- [0..n]])

------------ QUESTAO 3

--Analisa os algarismos de um numero, elimina os iguais a 1 e multiplica os restantes por uma potencia de 10
splitNumberCutOne :: Int -> Int -> Int
splitNumberCutOne n potdez
	| n >= 10 && (mod n 10) /= 1 = splitNumberCutOne (div n 10) (potdez*10) + (mod n 10)*potdez
	| n >= 10 && (mod n 10) == 1 = splitNumberCutOne (div n 10) potdez
	| n == 1 = 0
	| otherwise = n*potdez

--Gera uma nova lista com os numeros da lista original sem algarismos iguais a 1 (e nao nulos)
limpaUm :: [Int] -> [Int]
limpaUm [] = []
limpaUm (h:t) = [splitNumberCutOne x 1 | x <- (h:t), splitNumberCutOne x 1 /= 0]

------------ QUESTAO 4

inverteString :: String -> String
inverteString [] = []
inverteString (h:t) = inverteString t ++ [h]

comparaStrings :: String -> String -> Bool
comparaStrings [] [] = True
comparaStrings str1 str2
	| Data.Char.toLower(head str1) == Data.Char.toLower(head str2) = comparaStrings (tail str1) (tail str2)
	| otherwise = False

isPalindromo :: String -> Bool
isPalindromo str = comparaStrings str (inverteString str)

------------ QUESTAO 5

--Converte uma string contendo um numero binario para um inteiro na base decimal
btoi :: String -> Int
btoi [] = 0
btoi (h:t) = (2^(length (h:t) - 1)) * (Data.Char.digitToInt h) + btoi t