
vsemanais = [2, 2, 3, 4, 2, 3]


--funcao que retorna quantas casas das N casas de uma lista guardam o valor s
func1 :: Int -> Int -> Int
func1 s n 
	| (n == 0 && (vsemanais!!n) == s) = 1 
	| (n == 0 && (vsemanais!!n) /= s) = 0 
	| ((vsemanais!!n) == s) = func1 s (n-1) + 1 
	| otherwise = func1 s (n-1)


--funcao que percorre uma lista (do menor indice ao maior) e retorna o valor armazenado na posicao n
func2 :: Int -> [Int] -> Int
func2 n vsemanais 
	| vsemanais == [] = 0 
	| (n+1) == 1 = head vsemanais 
	| otherwise = func2 (n-1) (tail vsemanais)

--os arrays em haskell tambem possuem indice de 0 a X 