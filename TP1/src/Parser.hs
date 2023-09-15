module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "else", "repeat", "skip", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

----------------------------------
--- Parser de expressiones enteras
-----------------------------------

intexp :: Parser (Exp Int)
intexp =  eqexpr <|> intexp'  

eqexpr :: Parser (Exp Int)
eqexpr = try (do  v <- identifier lis
                  reservedOp lis "=" 
                  y <- intexp 
                  reservedOp lis ","
                  z <- intexp
                  return (ESeq (EAssgn v y) z) )

intexp' :: Parser (Exp Int)
intexp' = chainl1 factor addsub

factor:: Parser (Exp Int)
factor = chainl1 term divmul 

term:: Parser (Exp Int)
term = neg <|> var <|> nat <|> parens lis intexp

addsub:: Parser (Exp Int -> Exp Int -> Exp Int)
addsub = do reservedOp lis "+"
            return Plus
            <|> do reservedOp lis "-"
                   return Minus
divmul:: Parser (Exp Int -> Exp Int -> Exp Int)
divmul = do reservedOp lis "*"
            return Times
            <|> do reservedOp lis "/"
                   return Div                  

neg:: Parser (Exp Int)
neg = try(do reservedOp lis "-"
             x <- intexp
             return (UMinus x))

var:: Parser (Exp Int)
var = try (do v <- identifier lis
              return (Var v))

nat:: Parser (Exp Int)
nat = try (do n <- natural lis
              return (Const (fromIntegral n)))


-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = chainl1 conjunction orOp

conjunction :: Parser (Exp Bool)
conjunction = chainl1 (boolnegation <|> boolean <|> comparsion <|> parens lis boolexp) andOp

boolnegation :: Parser (Exp Bool)
boolnegation = try (do reservedOp lis "~"
                       b <- boolexp
                       return (Not b))

boolean:: Parser (Exp Bool)
boolean = do reserved lis "true"
             return BTrue
             <|> do reserved lis "false"
                    return BFalse
comparsion:: Parser (Exp Bool)
comparsion = try (do e1 <- intexp
                     op <- compareOp
                     e2 <- intexp
                     return (op e1 e2))

compareOp :: Parser (Exp Int -> Exp Int -> Exp Bool)
compareOp = do reservedOp lis "=="
               return Eq
               <|> do reservedOp lis "!="
                      return NEq
                      <|> do reservedOp lis ">"
                             return Gt
                             <|> do reservedOp lis "<"
                                    return Lt
orOp :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
orOp = do reservedOp lis "||"
          return Or

andOp :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
andOp = do reservedOp lis "&&"
           return And

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 (skip <|> assign <|> cond <|> repeat') semicolon

semicolon :: Parser (Comm -> Comm -> Comm)
semicolon = do reservedOp lis ";"
               return Seq

skip :: Parser Comm
skip = do reserved lis "skip"
          return Skip

assign :: Parser Comm
assign = do x <- identifier lis
            reserved lis "="
            i <- intexp
            return (Let x i)

cond :: Parser Comm
cond = do reserved lis "if"
          x <- boolexp
          y <- braces lis comm
          z <- ((reserved lis "else" >> braces lis comm) <|> skip)
          return (IfThenElse x y z)

repeat' :: Parser Comm
repeat' = do reserved lis "repeat"
             x <- braces lis comm
             reserved lis "until"
             y <- parens lis boolexp
             return (Repeat x y)

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
