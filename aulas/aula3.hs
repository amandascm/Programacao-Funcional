fat:: Int -> Int

fat n | n > 1 = n * fat (n-1) | n == 0 = 1 | otherwise = n * 1



all4Equal :: Int -> Int -> Int -> Int -> Bool

all4Equal a b c d 
    | a == b && b == c && c == d = True 
    | otherwise = False

equalCount :: Int -> Int -> Int -> Int 

equalCount a b c
    | a == b && b == c = 3
    | a == b = 2
    | b == c = 2
    | a == c = 2
    | otherwise = 0

-- determinar se dois numeros sao primos entre si 

mdc :: Int -> Int -> Int

mdc a b 
    | b /= 0 = mdc b (mod a b) 
    | otherwise = a


--fazer a funcao saoPrimos !!!!

double :: [Int] -> [Int]
double [] = []
double [x] = [2*x]
double (x:y) = [2*(x)] ++ double y

--membership :: [Int] -> Int -> Bool
membership [] a = False
membership [x] a 
    | x == a = True 
    | otherwise = False
membership (h:t) a 
    | h == a = True 
    | otherwise = membership t a

 
digits :: String -> String 
digits [] = []
digits [c] 
    | (membership ['0'..'9'] c) == True = [c] 
    | otherwise = []
digits (h:t) 
    | (membership ['0'..'9'] h) == True = [h] ++ digits t 
    | otherwise = [] ++ digits t

sumpairs :: [Int] -> [Int] -> [Int]
sumpairs [] a = a
sumpairs [] [] = []
sumpairs a b = [head a + head b] ++ sumpairs (tail a) (tail b)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

fiboPar :: Int -> [Int]
fiboPar 0 = []
fiboPar 1 = [] ++ fiboPar 0
fiboPar n 
    | mod (fib n) 2 == 0 = fiboPar (n-1) ++ [fib n]
    | otherwise = fiboPar (n-1)

fibo :: Int -> [Int]
fibo 0 = []
fibo n = fiboPar (n*3)

--QUICKSORT

ehDesordenado :: [Int] -> Bool
ehDesordenado [] = False
ehDesordenado [x] = False
ehDesordenado (h:t)
    | h <= (head t) = ehDesordenado t
    | h > (head t) = True

partMenor :: Int -> [Int] -> [Int]
partMenor p [] = []
partMenor p [x]
    | x <= p = [x]
    | otherwise = []
partMenor p (h:t)
    | h <= p = [h] ++ partMenor p t
    | otherwise = partMenor p t

partMaior :: Int -> [Int] -> [Int]
partMaior p [] = []
partMaior p [x]
    | x > p = [x]
    | otherwise = []
partMaior p (h:t)
    | h > p = [h] ++ partMaior p t
    | otherwise = partMaior p t

quick :: [Int] -> [Int]
quick [] = []
quick [x] = [x]
quick (h:t)
    | ehDesordenado ([h]++t) == True = quick ((partMenor h t) ++ [h] ++ (partMaior h t))
    | otherwise = [h]++t




