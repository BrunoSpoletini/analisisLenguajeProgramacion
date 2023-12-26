{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)
import System.Win32 (xBUTTON1, COORD (yPos))
--import GHC.Stack.CCS (whereFrom)
-- Practica 5

-- 1,2,3 en la carpeta


-- 4)
--a) 
liftA2::Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y

 
liftA5::Applicative f => (a -> b -> c -> d -> e -> k) -> f a -> f b -> f c -> f d -> f e -> f k
liftA5 f a b c d e = f <$> a <*> b <*> c <*> d <*> e
-- >>> liftA2 (,) (Just 3) (Just 5)
-- Just (3,5)

-- >>> liftA5 (\a -> \b -> \c -> \d -> \e -> (a++b,c+d+e)) (Just "Hola") (Just "Mundo") (Just 6) (Just 7) (Just 8)
-- Just ("HolaMundo",21)


--5)
sequenceAA :: Applicative f => [f a] -> f [a]
sequenceAA [] = pure []
sequenceAA (x:xs) = (:) <$> x <*> sequenceAA xs


--6)
--instance Monad m => Functor m where
--    fmap f x = x <<= return f
-- Al compi no le gusta
-- Demostracion en la carpeta

--7) 
newtype Id a = Id a
    deriving (Show)
data Either2 e a = Left2 e | Just2 a
    deriving (Show)
instance Functor Id where
    fmap = liftM

instance Applicative Id where
    pure = Id
    (<*>) = ap

instance Monad Id where
    return = pure
    (Id x) >>= f = f x

instance Functor (Either2 e) where
    fmap f (Left2 x) = Left2 x
    fmap f (Just2 x) = Just2 (f x)
instance Applicative (Either2 e) where
    pure = Just2
    _ <*> (Left2 x) = Left2 x
    (Just2 f) <*> (Just2 x) = Just2 (f x)

instance Monad (Either2 e) where
    return = pure
    (Left2 x) >>= _ = Left2 x
    (Just2 x) >>= f = f x

-- >>>(Just2 420) >>= (\x-> Just2 (x,69))
-- Just2 (420,69)

-- Comentado pq esto ya debe estar en el prelude
-- instance Monad [] where
--     return x = [x]
--     [] >>= f = [] 
--     (x:xs) >>= f =  f x ++ xs >>= f


-- 9)

data Expr a = Var a | Num Int | Add (Expr a) (Expr a) | Mul (Expr a) (Expr a)
    deriving (Show)
instance Functor Expr where 
    fmap = liftM
instance Applicative Expr where
    pure = Var
    (<*>) = ap
instance Monad Expr where 
    return = pure
    Var x >>= f = f x
    Num x >>= f = Num x
    Add x y >>= f = let l = x >>= f
                        r = y >>= f
                    in Add l r
-- >>>(Add (Num 10) (Var 1000)) >>= (\x -> Num (x-500))
-- Add (Num 10) (Num 500)

--But is it a monad tho?
{-
 (monad.1): return x >>= f = f x
    return x >> f = 
    Var x >>= f =
    f x =
    Var x 
 (monad.2): m >>= return = m
    Por induccion estructural
    Casos base:
    Var x >>= return = 
    return x =
    Var x OK

    Num x >>= return =
    Num x OK

    Caso inductivo:
    Asumimos cierto para x e y, y queremos probarlo para
    Add x y >>= return =
    Add (x >> = return) (y >>= return) = (HI)
    Add x y OK
 (monad.3): (m >>= f) >>= g = m >>= (\k -> f k >>= g)
    Por induccion, casos base:
    Var x >> (\k -> f k >>= g) =
    (\k -> f k >>= g) x =
    f x >>= g
    (Var x >>= f) >>= g OK

    Num x >> (\k -> f k >>= g) =
    Num x OK
    
    Paso inductivo: asumimos cierto para x e y
    Add x y >>= (\k -> f k >>= g) = 
    Add (x >>= (\k -> f k >>= g)) (y >>= (\k -> f k >>= g)) = (HI)
    Add ((x >>= f) >>= g) ((y >>= f) >>= g) =
    (Add (x >>= f) (y >>= f)) >>= g = 
    (Add x y >>= f) >>= g OK
-}

--b)
-- data Mul a = Mul (Expr2 a) (Expr2 a)

-- data Composed a = ExprAdd { exprAdd :: Expr }
--                 | ExprMul { exprMul :: Mul}

exprG :: [Char] -> Expr Int
exprG x = if x == "y" then 
                        Mul (Var 1) (Num 2) 
                    else
                        Var 1

-- >>> Add (Var "y") (Var "x") >>= exprG
-- Add (Mul (Var 1) (Num 2)) (Var 1)

-- Si existe una forma de hacerlo sin agregarle el mul al tipo de dato expr
-- estaria bueno saberlo

-- c
-- El operador >>= representa el reemplazo de variables 

--10
{-
do x ← (do z ←y
        w ←f z
        return (g w z ))
    y ← h x 3
    if y then return 7
        else do z ←h x 2
            return (k z)

(y >>= \z->
f z >>= \w ->
return (g w z)) >>= \x ->
    h x 3 >>= \y ->
        if y then return 7
        else (h x 2 >>= \z ->
              return (k z)
-}
--11 Escribir el siguiente fragmento de programa
--  monadico usando notacion do.
{-
(m >>= λx → h x ) >>= λy →
f y >>= λz → 
return (g z )

do y <- (do     x <- m
                h x)
    z <- f y
    return (g x)
CHECKEAR
-}
--12 Escribir las leyes de las moandas usando notacion do
{-

(monad.1): return x >>= f = f x
do  q <- return x
    f q
= f x

(monad.2): m >>= return = m
m >>= \q ->
return q --aux

do q <- m
   return q
= m

(monad.3): m >>= (\k -> f k >> g) = (m >>= f ) >>= g

--m >>= (\k -> f k >>= (\q -> g q)) aux

do k <- m
   q <- f k
   g q

=

--(m >>= (\q -> f q)) >>= (\e -> g e) aux

do e <- (do q <- m
            f q)
    g e
   
-}

--13
{-
La clase Monoid clasifica los tipos que son monoides y
 esta definida de la siguiente manera
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m

-}

-- a) Probar que string es un monoide
{-
instance Monoid [] where
    mempty = []
    mappend xs ys  = xs ++ ys

(monoid.1) mappend-asociativa
mappend x (mappend y z) = 
x ++ (mappend y z) =
x ++ (y ++ z) =
(x ++ y) ++ z =
(mappend x y) mappend z OK

(monoid.2) mempty elem neutro por izq y por der de mappend
mappend mempty y = [] ++ y = y
mappend x mempty = x ++ [] = x OK
... String es un monoide

b) Probar que Output es una monada asumiendo que w es un monoide
-}  
newtype Output w a = Out(a, w)
    deriving (Show)

instance (Monoid m) => Functor (Output m) where
    fmap f (Out (x,w)) = Out (f x, w )

instance (Monoid m) => Applicative (Output m) where
    pure x = Out (x,mempty)
    (Out (f,w1)) <*> (Out (x, w2)) = Out (f x, mappend w1 w2)

instance (Monoid m) => Monad (Output m) where
    return = pure
    (Out (x, w)) >>= f = Out (id, w) <*> f x

-- >>> Out(100, "Text") >>= (\x -> Out(x*3,"Test"))
-- Out (300,"TextTest")

--b) Probar que Output m a es una monada PENDIENTE

-- d)
write ::Monoid w => w -> Output w ()
write x = Out ((), x)

--e) 

data Exp a =  ELit Int -- enteros
            | EVar String -- variables
            | EAdd (Exp a) (Exp a) -- sumas
            | EDiv (Exp a) (Exp a)
            
    deriving (Show)

type Env = String -> Int

newtype M a = M { runM :: Env -> Maybe (a , Int) }


instance Functor M where
    fmap = liftM

instance Applicative M where
    pure x = M (\ _ -> Just (x , 0))
    (<*>) = ap

instance Monad M where
    return = pure
    M h >>= f =
        M (\ e -> case h e of
                    Nothing -> Nothing
                    Just (a , m ) -> case   runM ( f a ) e of
                                            Nothing -> Nothing
                                            Just (b , n ) -> Just (b , m + n ) )




-- lanza error
throw :: M a
throw = M (\ _ -> Nothing )
-- obtiene entorno
ask :: M Env
ask = M (\ e -> Just (e , 0) )
-- acumula 1
tick :: M ()
tick = M (\ _ -> Just (() , 1) )


eval :: Exp v -> M Int
eval ( ELit n ) = return n
eval ( EVar v ) = do e <- ask
                     return ( e v )
eval ( EAdd t u ) = do  x <- eval t
                        y <- eval u
                        return ( x + y )
eval ( EDiv t u ) = do  x <- eval t
                        y <- eval u
                        if y == 0
                        then throw
                        else return (div x y )

res = do eval(EAdd (ELit 10) (ELit 5))
res2 = runM res (\s -> 0)




-- newtype M a = M { runM :: Env -> Maybe (a , Int) }

-- newtype Super w a = Eout { runS :: ( M a, w) }

-- instance Functor (Super String) where
--     fmap = liftM

-- instance Applicative (Super String) where
--     pure = return
--     (<*>) = ap

-- instance Monad (Super String) where
--     return x = Eout(M (\ _ -> Just (x , 0)) , [] )
--     Eout (M h, w) >>= f =
--         Eout (M (\ e -> case h e of
--                     Nothing -> Nothing
--                     Just (a , m ) ->  let (m2, w2) = runS(f a)
--                                         in  (case runM m2 e of
--                                                     Nothing -> Nothing
--                                                     Just (b , n ) -> Just (b , m + n )) ), w++w2)

escribe :: String -> Output String Int
escribe s = Out (0, s)

getData :: Exp v -> String
getData ( ELit n ) = "(Lit "++ show n ++ ")"
getData ( EAdd t u) = "(Add "++ getData t ++ " " ++ getData u ++ ")"
getData ( EDiv t u ) = "(Div "++ getData t ++ " " ++ getData u ++ ")"

evalS :: Exp v -> Output String Int
evalS e@( ELit n ) = do escribe ("El termino " ++ getData e ++ " tiene valor " ++ show n ++"\n")
                        return n
evalS e@( EAdd t u ) = do   x <- evalS t
                            y <- evalS u
                            escribe ("El termino " ++ getData e ++ " tiene valor " ++ show (x + y) )
                            return (x+y)

evalS e@( EDiv t u ) = do   x <- evalS t
                            y <- evalS u
                            escribe ("El termino " ++ getData e ++ " tiene valor " ++ show (x + y) )
                            return (div x y )


-- >>> evalS(EAdd(ELit 14) (ELit 2))

{-
--14) Sea M una monada. Dados los operadores:

(>>) :: M a → M b → M b
(>>=) :: M a → (a → M b) → M b

M a → M b → M b
x >> y = x >>= (\_ -> y)

M a → (a → M b) → M b
M x >>= f =  (M x) >> f x //Pero no se puede desencapsular (M a) de forma general, por lo q esto no es posible

-}

--15)
--a)
mapAux::Monad m => [m b] -> m [b]
mapAux [] = return []
mapAux l@(x:xs) = do    b <- x
                        bs <- mapAux xs
                        return (b:bs)


mapMM::Monad m => (a -> m b) -> [a] -> m [b]
mapMM f [] = return []
mapMM f l@(x:xs) = mapAux (map f l)

-- >>> mapMM (\x -> Just (x+1)) ([10, 20])

--b)
foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM f xf [x] = f xf x
foldM f x0 l@(x:xs) = do    e <- f x0 x
                            foldM f e xs

-- >>> foldM (\x -> \y -> Just (x+y)) 10 [100, 200,300]






