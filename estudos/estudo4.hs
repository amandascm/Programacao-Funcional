import Control.Monad
import Prelude hiding (Maybe (..))
import Data.Char

--recursao de cauda: nao usar o retorno da funcao como parte do resultado de uma mesma funcao para evitar estouro de pilha
--passar o resultado atual como parametro para a proxima chamada (nao mantem todas as chamadas num stackframe)

split :: String -> [String]
split [] = []
split (h:t) = splitWords (h:t) []

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

--funcao que retorna lista de sequencias de letras em uma string usando recursao de cauda
splitWords :: String -> [String] -> [String]
splitWords [] list = list
splitWords (h:t) list
    | h == ' ' = splitWords (dropSpace (h:t)) list
    | otherwise = splitWords (dropWord (h:t)) ([getWord (h:t)]++list)

--IO
writeHello :: IO ()
writeHello = putStr "Hello"

{---com main para compilar codigo com ghc
main = do 
        --a funcao getline retorna um IO String, nao uma String (o 'input' tem tipo String)
        input <- getLine
        --a setinha usada com acoes de IO eh como uma renomeacao do retorno da acao, nao uma atribuicao
        writeHello
        --nao poderia fazer 'putStrLn (getLine ++ "!")
        putStrLn (" " ++ input ++ "!")-}


data Maybe a = Just a |
               Nothing
               deriving(Show)

safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond (h:t)
    | length (h:t) >= 2 = Just (head t)
    | otherwise = Nothing

safeCalc :: String -> IO ()
safeCalc [] = putStr []
safeCalc str
    | (getOp str) == "sum" = putStrLn (show (Just ((read (getNumb str) :: Int) + (read (reverse (getNumb (reverse str))) :: Int))))
    | (getOp str) == "sub" = putStrLn (show (Just ((read (getNumb str) :: Int) - (read (reverse (getNumb (reverse str))) :: Int))))
    | (getOp str) == "div" = putStrLn (show (safeDiv (read (getNumb str) :: Int) (read (reverse (getNumb (reverse str))) :: Int)))
    | otherwise = putStrLn (show (Just ((read (getNumb str) :: Int) * (read (reverse (getNumb (reverse str))) :: Int))))

safeDiv _ 0 = Nothing
safeDiv x y = Just (div x y)

getNumb [] = []
getNumb (h:t)
    | (ehDigito h) || h == '-' = [h]++(getNumb t)
    | otherwise = []

getOp str = [c | c <- str, not (ehDigito c), c /= '-']

--diz se um char eh um digito (0..9)
ehDigito :: Char -> Bool
ehDigito c
    | ord c <= ord '9' && ord c >= ord '0' = True
    | otherwise = False