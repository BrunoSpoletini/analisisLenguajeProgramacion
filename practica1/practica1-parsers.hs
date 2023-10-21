import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)


{-
Ejercicio 1: estudiar las directivas de Parsing.hls

¿Que hace sepBy? 
Separa la cadena segun como la divide el parser 2,
leyendo cada tramo con el parser 1
¿Que hace symbol?
parse (symbol "hola") "holaawd" -> [("hola","awd")]


ghci practica1-parsers.hs

parse (sat es7) "hola" -> []]
parse (sat es7) "7" -> [('7',"")]
parse (string "Ho") "Hola" -> [("Ho","la")]

parse (many (string "Ho")) "HoHoHola" -> [(["Ho","Ho","Ho"],"la")]


parse (sepBy (letter) (string "o") ) "HoHoHola" -> [("HHHl","a")]

-}
es7 :: Char -> Bool 
es7 x = x == '7'


-- EJERCICIO 2 - 8

{-
expr → expr ('+' term | '-' term) | term
term → term ('*' factor | '/' factor) | factor
factor → digit | '(' expr ')'
digit → '0' | '1' | · · · | '9'

Sin recursion a izquierda:
expr → term expr'
expr' -> ('+' expr | '-' expr | e)

term → factor term'
term' -> ('*' term | '/' term | e)

factor → digit | '(' expr ')'
digit → '0' | '1' | · · · | '9'

Formula general para resolver recursion a izquierda
A -> Aa | b
Se lleva a:
A -> bA'
A' -> e | aA'

-}

-- asociando a derecha
-- expr :: Parser Int
-- expr =  do  t <- term
--             (do char '+'
--                 e <- expr
--                 return (t + e)
--                 <|> (do char '-'
--                         e <- expr
--                         return (t - e) 
--                         <|>
--                         return t) )

  
-- term :: Parser Int
-- term = do   f <- factor
--             (do char '*'
--                 t <- term
--                 return (f * t)
--                 <|> (do char '/'
--                         t <- term
--                         return (div f t)
--                         <|> return f) )

-- Sin recursion a izquierda (y asociando a izquierda?):
-- expr :: Parser Int
-- expr =  do  t <- term
--             e <- (expr2 t)
--             return e
--         <|> term

-- expr2 :: Int -> Parser Int
-- expr2 x =  do   char '+'
--                 e <- expr
--                 return (x + e)
--             <|> (do char '-'
--                     e <- expr
--                     return (x - e)
--                 <|> return x)
  
-- term :: Parser Int
-- term = do   f <- factor
--             (do char '*'
--                 t <- term
--                 return (f * t)
--                 <|> (do char '/'
--                         t <- term
--                         return (div f t)
--                         <|> return f) )

-- factor :: Parser Int
-- factor = do d <- digit
--             return (digitToInt d)
--             <|>   (do    char '('
--                          e <- expr
--                          char ')'
--                          return e)

-- eval :: String -> Int
-- eval xs = fst (head (parse expr xs))


-- EJERCICIO 3 --
-- Escribir un transformador que al recibir un parser,
-- devuelva un nuevo parser que se comporta como el original
-- pero que tambien acepta opcionalmente que las cadenas esten entre parentesis.
transformador :: Parser a -> Parser a
transformador p =   do          a <- char '('
                                b <- p
                                c <- char ')'
                                return b
                        <|> p

-- EJERCICIO 4 --

{-
Modificar el parser del ejercicio 2 para que en lugar de evaluar una expresion genere un arbol de sintaxis
abstracta dado por el tipo:
data Expr = Num Int | BinOp Op Expr Expr
data Op = Add | Mul | Min | Div

expr -> term | op term expr
term -> factor | op factor term
op -> Add | Mul | Min | Div

-}
data Expr = Num Int | BinOp Op Expr Expr
        deriving (Show)
data Op = Add | Mul | Min | Div
        deriving (Show)


expr :: Parser Expr
expr =  do  t <- term
            (do char '+'
                e <- expr
                return (BinOp Add t e)
                <|> (do char '-'
                        e <- expr
                        return (BinOp Min t e) 
                        <|>
                        return t) )

term :: Parser Expr
term = do   f <- factor
            (do char '*'
                t <- term
                return (BinOp Mul f t)
                <|> (do char '/'
                        t <- term
                        return (BinOp Div f t)
                        <|> return f) )

factor :: Parser Expr
factor = do d <- digit
            return (Num (digitToInt d))

eval :: String -> Expr
eval xs = fst (head (parse expr xs))

-- EJERCICIO 5 --

data Basetype = DInt | DChar | DFloat
        deriving (Show)
type Hasktype = [Basetype]

-- typeL -> type | typeL -> type
-- type -> DInt | DChar | DFloat
 
-- "Int -> Char -> Float"

type5 :: Parser Basetype
type5 = do      x <- string "Int"
                return DInt
        <|> do  x <- string "Char"
                return DChar
        <|> do  x <- string "Float"
                return DFloat

typeL :: Parser Hasktype
typeL = do t <- type5
           do   space
                string "->"
                space
                rest <- typeL
                return (t:rest)
                <|> return [t]

-- EJERCICIO 6 --
{-
Escribir un parser para listas heterogeneas de enteros y caracteres por 
extension usando el formato de Haskell.
Defina un tipo de datos adecuado para representar estas listas parseadas.
Por ejemplo, una cadena a parsear es la siguiente:
[1,'a','b',2,3,'c']
-}

data HType = DInt2 Int| DChar2 Char
        deriving (Show)
type HList = [HType]

hList :: Parser HList
hList = do      char '['
                r <- readHet
                res <- many (do         char ','
                                        r <- readHet
                                        return r )
                char ']'
                return res


readHet :: Parser HType
readHet = do    n <- int
                return (DInt2 n)
                <|> do  char '\''
                        l <- letter
                        char '\''
                        return (DChar2 l)

-- EJERCICIO 7 --

{-
Gramatica:
Hasktype2 -> Basetype | Fun Hasktype2 Hasktype2
Basetype -> DInt | DChar | DFloat
-}

-- "Int -> Char -> Float"
-- Fun DInt (Fun DChar DFloat)


data Hasktype2 = Basetype | Fun Hasktype2 Hasktype2
-- data Basetype = DInt | DChar | DFloat --Ya esta declarado arriba
haskType :: Parser Hasktype2
haskType = do  t <- type5
               (do      space
                        string "->"
                        space
                        rest <- haskType
                        return (Fun t rest)
                        <|> (return (t::Hasktype2))) -- t es tipo Basetype, que es un componente de Hasktype2, la vdd no se pq no anda ni como fixearlo

-- EJERCICIO 8 --
-- El problema de la recursion a izq es que no termina nunca:
-- expr -> expr (..) queda en un loop infinito
{-
Gramatica: CON RECURSION A IZQ
expr -> expre ('+' term | '-' term) | term
term -> term ('*' factor | '/' factor) | factor
factor -> digit | '(' expr ')'
digit -> '0' | '1' | · · · | '9'

Gramatica: SIN RECURSION A IZQ
expr -> term expr'
expr' -> e | ('+' term | '-' term) expr'
term -> factor term'
term' -> e | ('*' factor | '/' factor) term'
factor -> digit | '(' expr ')'
digit -> '0' | '1' | · · · | '9'

-}

-- x-(y-z) -> mal
-- (x-y)-z -> bien
-- 8 - 2 - 1 -> 5
-- HECHO EN CLASE
expr :: Parser Int
expr = do       t <- term
                f <- expr'
                return (f t)

expr' :: Parser (Int -> Int)
expr' = do      char '+'
                t <- term
                f <- expr'
                return ( \x -> f(x + t) )
                <|>     (do char '-'
                        t <- term
                        f <- expr'
                        return ( \x -> f(x - t) )
                        <|> return (\x -> x))

