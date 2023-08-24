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

{-
expr → expr ('+' term | '-' term) | term
term → term ('*' factor | '/' factor) | factor
factor → digit | '(' expr ')'
digit → '0' | '1' | · · · | '9'
-}
expr :: Parser Int
expr =  do  t <- term
            (do char '+'
                e <- expr
                return (t + e)
                <|> (do char '-'
                        e <- expr
                        return (t - e) 
                        <|>
                        return t) )

  
term :: Parser Int
term = do   f <- factor
            (do char '*'
                t <- term
                return (f * t)
                <|> (do char '/'
                        t <- term
                        return (div f t)
                        <|> return f) )

factor :: Parser Int
factor = do d <- digit
            return (digitToInt d)
            <|>   (do    char '('
                         e <- expr
                         char ')'
                         return e)

eval :: String -> Int
eval xs = fst (head (parse expr xs))