data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

--robo 1

destination :: (Int,Int) -> [Command] -> (Int,Int)
destination coords [] = coords
destination coords commands = dest coords (0,1) commands

dest :: (Int,Int) -> (Int, Int) -> [Command] -> (Int, Int)
dest coords orient [] = coords
dest coords orient ((Forward n):t) = dest (forward coords orient n) orient t
dest coords orient ((Backward n):t) = dest (backward coords orient n) orient t
dest coords orient ((TurnLeft):t) = dest coords (turnleft orient) t
dest coords orient ((TurnRight):t) = dest coords (turnright orient) t

forward :: (Int, Int) -> (Int, Int) -> Int -> (Int, Int)
forward coords orient 0 = coords
forward coords orient n
    | fst orient == 0 && snd orient == 1 = (fst coords, (snd coords)+n)
    | fst orient == 0 && snd orient == -1 = (fst coords, (snd coords)-n)
    | fst orient == 1 && snd orient == 0 = ((fst coords)+n, snd coords)
    | otherwise = ((fst coords)-n, snd coords)

backward :: (Int, Int) -> (Int, Int) -> Int -> (Int, Int)
backward coords orient 0 = coords
backward coords orient n
    | fst orient == 0 && snd orient == 1 = (fst coords, (snd coords)-n)
    | fst orient == 0 && snd orient == -1 = (fst coords, (snd coords)+n)
    | fst orient == 1 && snd orient == 0 = ((fst coords)-n, snd coords)
    | otherwise = ((fst coords)+n, snd coords)

turnleft :: (Int, Int) -> (Int, Int)
turnleft orient
    | fst orient == 0 && snd orient == 1 = (-1, 0)
    | fst orient == 0 && snd orient == -1 = (1, 0)
    | fst orient == 1 && snd orient == 0 = (0, 1)
    | otherwise = (0, -1)

turnright :: (Int, Int) -> (Int, Int)
turnright orient
    | fst orient == 0 && snd orient == 1 = (1, 0)
    | fst orient == 0 && snd orient == -1 = (-1, 0)
    | fst orient == 1 && snd orient == 0 = (0, -1)
    | otherwise = (0, 1)

--robo 2

faces :: Direction -> [Command] -> Direction
faces dir [] = dir
faces orient ((Forward n):t) = faces orient t
faces orient ((Backward n):t) = faces orient t
faces orient ((TurnLeft):t) = faces (turnleftt orient) t
faces orient ((TurnRight):t) = faces (turnrightt orient) t

turnleftt :: Direction -> Direction
turnleftt North = West
turnleftt West = South
turnleftt South = East
turnleftt East = North

turnrightt :: Direction -> Direction
turnrightt North = East
turnrightt East = South
turnrightt South = West
turnrightt West = North



--arvore de busca binaria 1
data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read, Show)

isBST :: Ord t => Tree t -> Bool
isBST Nilt = True 
isBST (Node n t1 t2) = ((isMinor n t1) && (isMajor n t2) && (isBST t1) && (isBST t2))

isMinor v Nilt = True
isMinor v (Node n t1 t2)
    | v > n = True && (isMinor v t1) && (isMinor v t2)
    | otherwise = False

isMajor v Nilt = True
isMajor v (Node n t1 t2)
    | v < n = True && (isMajor v t2) && (isMajor v t1)
    | otherwise = False

--arvore de busca binaria 2
insertList :: Ord t => Tree t -> [t] -> Tree t
insertList Nilt [] = Nilt
insertList t [] = t
insertList t list = insertList (insertt (head list) t) (tail list)


insertt :: Ord t => t -> Tree t -> Tree t
insertt v Nilt = Node v Nilt Nilt
insertt v (Node n t1 t2)
    | v < n = Node n (insertt v t1) t2
    | v > n = Node n t1 (insertt v t2)
    | otherwise = Node n t1 t2

--editor de texto
data Cmd = Cursor Int
           | Backspace Int
           | Delete Int
           | Insert String
           deriving (Read)

editText :: String -> [Cmd] -> String
editText str [] = str
editText str comandos = edit str 0 comandos

edit :: String -> Int -> [Cmd] -> String
edit str cursor [] = str
edit str cursor (Cursor x:comandos) = edit str (cursor + x) comandos
edit str cursor (Backspace x:comandos) = edit (backspace str cursor x) (cursor - x) comandos
edit str cursor (Delete x:comandos) = edit (delete str cursor x) (cursor) comandos
edit str cursor (Insert x:comandos) = edit (insert str cursor x) (cursor) comandos

substring :: String -> Int -> String
substring [] x = []
substring str 0 = []
substring str x = (head str):(substring (tail str) (x-1))

dropstring :: String -> Int -> String
dropstring [] x = []
dropstring str 0 = str
dropstring str x = dropstring (tail str) (x-1)
-- amanda c=4 2 = (getsubstring str (c-x))--am ++ (dropstring str c)--da
backspace :: String -> Int -> Int -> String
backspace [] c x = []
backspace str c 0 = str
backspace str c x = (substring str (c-x)) ++ (dropstring str c)
--amanda c=2 2 = (substring str (c))--am ++ (dropstring str (c+x))
--talvez precise fazer cursor-1 se deletar todos os elementos a frente do cursor (ele fica apontando para um caractere vazio)
delete :: String -> Int -> Int -> String
delete [] c x = []
delete str c 0 = str
delete str c x = (substring str c) ++ (dropstring str (c+x))
--amanda c=2 "oy" = (substring str c) ++ str2 ++ (dropstring str c)
insert :: String -> Int -> String -> String
insert str c [] = str
insert str c str2 = (substring str c) ++ str2 ++ (dropstring str c)