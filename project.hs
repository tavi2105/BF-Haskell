import Data.Char

-- lista limbaj
data BF = END 
    | Plus (BF)    -- "+"
    | Minus (BF)   -- "-"
    | Next (BF)    -- ">"
    | Back (BF)    -- "<"
    | Do (BF)      -- "["
    | While (BF)   -- "]"
    | Write (BF)   -- "."
    | Read (BF)    -- ","
    deriving(Show)


data List = NULL | Val Int (List) deriving(Show) -- lista elemente

data Validator = NullExp | Invalid | WrongLoopEnd | WrongLoopBegin | WrongShift | Good deriving(Eq,Show)

-- update list - primeste pozitia si valoarea
update :: List -> Int -> Int -> Int -> List 
update NULL a b c = (Val c (NULL))
update (Val s (x)) a b c = if a == b then (Val c (x)) else (Val s (update x a (b+1) c))


-- get element by position
get :: List -> Int -> Int -> Int
get NULL a b = 0
get (Val s (x)) a b = if a == b then s else (get x a (b+1))

-- returneaza capul listei
headd :: [BF] -> BF
headd (x:lista) = x

-- returneaza lista fara elemetul din capat
popp :: [BF] -> [BF]
popp (x:lista) = lista

-- returneaza lista impreuna cu noul element in cap
pushh :: [BF] -> BF-> [BF]
pushh lista x = ([x]++lista)

-- din char in string
charToString :: Char -> String
charToString c = [c]

-- mother ship function
expression :: BF -> Int -> Int -> List -> [BF] -> IO ()
expression END val pos lista stiva = do
    putStrLn ""
expression (Plus a) val pos lista stiva = expression a (val + 1) pos lista stiva
expression (Minus a) val pos lista stiva = expression a (val - 1) pos lista stiva
expression (Next a) val pos lista stiva = expression a (get lista (pos+1) 0) (pos+1) (update lista pos 0 val) stiva
expression (Back a) val pos lista stiva = expression a (get lista (pos-1) 0) (pos-1) (update lista pos 0 val) stiva
expression (Do a) val pos lista stiva = expression a val pos lista (pushh stiva a)
expression (While a) val pos lista stiva = if val == 0 then expression a val pos lista (popp stiva) 
                                                       else expression (headd stiva) val pos lista stiva
expression (Write a) val pos lista stiva = do
        putStr $ id (charToString (chr val))
        expression a val pos lista stiva
expression (Read a) val pos lista stiva = do
        putStrLn ""
        value <- getLine
        let intValue = read value :: Int
        expression a intValue pos lista stiva

-- main function
brainfck :: String -> IO ()
brainfck x = if ((validate x 0 0) == Invalid) then do
        putStrLn "ERROR: Not an Brainf*uck expression!"
    else if ((validate x 0 0) == NullExp) then do
        putStrLn "ERROR: Null expression!"
    else if ((validate x 0 0) == WrongShift) then do
        putStrLn "ERROR: Wrong element access."
    else if ((validate x 0 0) == WrongLoopEnd) then do
        putStrLn "ERROR: Loop ends but never begins."
    else if ((validate x 0 0) == WrongLoopBegin) then do
        putStrLn "ERROR: Loop begins but never ends."
    else expression (parse x) 0 0 NULL [] 
 

-- parseaza stringul primit si il transforma in lista BF pe care o pot porcesa
parse :: String -> BF 
parse "+" = (Plus END)
parse "-" = (Minus END)
parse ">" = (Next END)
parse "<" = (Back END)
parse "[" = (Do END)
parse "]" = (While END)
parse "." = (Write END)
parse "," = (Read END)
parse ('+':x) = (Plus (parse x))
parse ('-':x) = (Minus (parse x))
parse ('>':x) = (Next (parse x))
parse ('<':x) = (Back (parse x))
parse ('[':x) = (Do (parse x))
parse (']':x) = (While (parse x))
parse ('.':x) = (Write (parse x))
parse (',':x) = (Read (parse x))


-- valideaza codul
validate :: String -> Int -> Int -> Validator 
validate "" y z = NullExp
validate "+" y z = if y == 0 then Good else WrongLoopBegin
validate "-" y z = if y == 0 then Good else WrongLoopBegin
validate ">" y z = if y == 0 then Good else WrongLoopBegin
validate "<" y z = if z-1 < 0 then WrongShift else (if y == 0 then Good else WrongLoopBegin)
validate "[" y z = WrongLoopBegin
validate "]" y z = if y-1 == 0 then Good else (if y-1 < 0 then WrongLoopEnd else WrongLoopBegin)
validate "." y z = if y == 0 then Good else WrongLoopBegin
validate "," y z = if y == 0 then Good else WrongLoopBegin
validate ('+':x) y z = (validate x y z)
validate ('-':x) y z = (validate x y z)
validate ('>':x) y z = (validate x y (z+1))
validate ('<':x) y z = if z-1 < 0 then WrongShift else (validate x y (z-1))
validate ('[':x) y z = (validate x (y+1) z)
validate (']':x) y z = if y-1 < 0 then WrongLoopEnd else (validate x (y-1) z)
validate ('.':x) y z = (validate x y z)
validate (',':x) y z = (validate x y z)
validate _ y z = Invalid
 
expr0 = "++->++>+"

expr1 = "++->++>+<"

expr2 = "++->++>+<+"

expr3 = "++->++>+<+<-"

expr4 = "++->++>+<+<->>+"

expr5 = "++++[-]+"

expr6 ="++++[->+<]+"

expr7 = "++++[->+++[-]+<-]+"

hello = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

expr8 = "[,.]."

expr9 = "[,+++.---]."

expr10 = "[.+]"