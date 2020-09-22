dropSpace :: String -> String
dropSpace [] = []
dropSpace (h:t)
    | h == ' ' = dropSpace t
    | otherwise = (h:t)

dropWordSpace :: String -> String
dropWordSpace [] = []
dropWordSpace (h:t)
    | h /= ' ' = dropWordSpace t
    | otherwise = (h:t)

getWordSpace :: String -> String
getWordSpace [] = []
getWordSpace (h:t)
    | h /= ' ' = [h] ++ getWordSpace t
    | otherwise = []

splitWords :: String -> [String]
splitWords [] = []
splitWords (h:t)
    | h == ' ' = splitWords (dropSpace (h:t))
    | otherwise = [getWordSpace (h:t)] ++ splitWords (dropWordSpace (h:t))

listMonths [] n = []
listMonths (h:t) n 
  | n == 3 = (listMonths t 1)
  | n == 2 = (listMonths t 3)
  | n == 1 = [last (splitWords h)]++(listMonths t 2)
  | otherwise = []

getWord (h:t)
  | h /= ';' = [h]++getWord t 
  | otherwise = []
  
dropWord [] = []
dropWord (h:t)
    | h /= ';' = dropWord t
    | otherwise = t

listWords [] = []
listWords (h:t) = [(getWord (h:t))] ++ listWords (dropWord (h:t))

listValues [] n = []
listValues (h:t) n 
  | n == 3 = [read h :: Double] ++ (listValues t 1)
  | n == 2 = (listValues t 3)
  | n == 1 = (listValues t 2)
  | otherwise = []

getValues :: [Double] -> [String] -> String -> [Double]
getValues [] [] mes = []
getValues (h1:t1) (h2:t2) mes
    | h2 == mes = [h1] ++ (getValues t1 t2 mes)
    | otherwise = (getValues t1 t2 mes)

somaVal :: (Num a) => [a] -> a  
somaVal = foldl (+) 0

logMes :: String -> String -> Double
logMes mes [] = 0
logMes mes str
  | mes == "JAN" || mes == "FEV" || mes == "MAR" || mes == "ABR" = somaVal (getValues (listValues (listWords str) 1) (listMonths (listWords str) 1) mes)
  | otherwise = 0
main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result


findChar :: Char -> [(Char, Char)] -> Char
findChar c [] = c
findChar c (h:t)
  | fst h == c = snd h
  | otherwise = findChar c t

translate :: [Char] -> [(Char, Char)] -> String
translate [] l2 = []
translate l1 [] = []
translate (h1:t1) l2 = [(findChar h1 l2)] ++ (translate t1 l2)


decEnigma :: String -> [(Char, Char)] -> String
decEnigma l [] = l
decEnigma [] alp = []
decEnigma str alp = translate str alp