{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use replicateM" #-}
{-# HLINT ignore "Use second" #-}
module Main where



    import Control.Concurrent
    import Data.Char
    import Control.Applicative 
    import Control.Monad (liftM, ap)
    -- 1)
    helloW :: IO()
    helloW = do     putStrLn "Hola mundo!"
    main :: IO()
    main = helloW

    -- 2)
    getChars :: Int -> IO String
    getChars x = sequenceA (replicate x getChar)

    --3)
    magicNum = 7

    guessNum :: IO ()
    guessNum = do   x <- getLine
                    -- print x 
                    if read x == magicNum then
                        putStrLn "Win!"
                    else
                        if read x < magicNum then
                            do  putStrLn "Try higher"
                                guessNum
                        else
                            do  putStrLn "Try lower"
                                guessNum



    --4)
    manager :: ([Int], Int)
    manager = ([5, 4, 3, 2, 1],0)
    nim :: ([Int],Int) -> IO ()
    nim manager =do showBoard (fst manager)
                    putStrLn ("Turno jugador " ++ show (snd manager + 1))
                    putStrLn "Inserte el numero de fila a la cual le va a sacar estrellas:"
                    r <- getLine
                    putStrLn "Inserte el numero de estrellas a quitar"
                    c <- getLine
                    let newBoard = processMove (read r) (read c) (fst manager)
                        in if foldr min 5 newBoard == 0 then
                                putStrLn ("Gano el jugador " ++ show (snd manager + 1))
                            else nim (newBoard, 1 - snd manager) -- the game keeps going changing the turn
                     
    showBoard :: [Int] -> IO ()
    showBoard [] = putStrLn ""
    showBoard (x:xs) = do   putStrLn (concat (replicate x "* "))
                            showBoard xs

    processMove :: Int -> Int -> [Int] -> [Int]
    processMove r c board = let value = board !! (r-1)
                                in if value - c <= 0 then
                                        [0]
                                    else 
                                        take (r-1) board ++ [value-c] ++ drop r board


    --5)
    uppercase :: String -> String
    uppercase = map toUpper

    readFromFile :: IO ()
    readFromFile = do   file <- readFile "./archivoLectura.txt" 

                        writeFile "./arhivoSalida.txt" (uppercase file ++ "\n")


    {-
    Lee un caracter del teclado, lo imprime en la pantalla y lo
    devuelve como valor.
    I getChar :: IO Char

    Muestra un caracter en la pantalla.
    I putChar :: Char -> IO ()

    I putStr :: String -> IO ()

    I putStrLn :: String -> IO ()

    I getLine :: IO String

    Lee el contenido de un archivo y lo devuelve
    I readFile :: FilePath -> IO String

    
    -}


    --6)
    newtype State s a = St {runState :: s -> (a,s)}

    instance Functor (State s) where
        fmap = liftM
    
    instance Applicative (State s) where
        pure = return
        (<*>) = ap

    instance Monad (State s) where
        return x = St (\s ->(x ,s))
        (St h) >>= f = St (\s ->let (x ,s') = h s   
                                  in runState (f x ) s')

    --a)
    --(monad.1): return k >>= f = f k
    {-
    return k >>= f = (def ret)
    St(\g -> (k, g)) >>= f =

    St(\s -> let (x, s') = (\g -> (k, g)) s
            in runState (f x) s') =

    St(\s -> let (x, s') = (k, s) 
            in runState (f x) s') =
    
    St(\s -> runState (f k) s) = (def abstraccion)

    f k
    
    -- (monad.2): (St g) >>= return = (St g)
    (St g) >>= return =
    St (\s -> let (x ,s') = g s   
               in runState (return x ) s') =

                in runState (St (\s'' -> (x, s'')) s')
                in (\s'' -> (x, s'')) s'
                in (x,s')

    ..todo el let devuelve (g,s)..

    St (\s -> let (x ,s') = g s   
                in   (x ,s'))   ))
    St (\s ->  (g, s) ) =
    St g

    -- (monad.3): (m >>= f) >>= g = m >>= (\k -> f k >>= g)
    -- imposible
    -}

    --b)

    set :: s -> State s ()
    set x = St (\s -> ((), x))

    get:: State s s
    get = St (\s -> (s,s))

    --7)
    data Cont r a = Cont{ runCont :: (a -> r) -> r }

    instance Functor (Cont r) where
        fmap = liftM

    instance Applicative (Cont r) where
        pure = return
        (<*>) = ap

    instance Monad (Cont r) where
        return x = Cont (\g ->  g x )
        (Cont h) >>= f = Cont (\g -> h (\a -> runCont(f a) g)  )
            
    -- Pendiente probar que es una monada

    --8)
    

    data T = Con Int | Div T T
    
    data Error a b  = Izq a | Der b


    newtype M2 s e a = M2 {runM :: s -> Error e (a, s)}


    eval :: T -> M2 Int String Int
    eval (Con n) = return n
    eval (Div t1 t2) = do   v1 <- eval t1
                            v2 <- eval t2
                            if v2 == 0 then raise "Error: Division por cero."
                            else do modify (+1)
                            return (div v1 v2)

    doEval :: T -> Error String (Int, Int)
    doEval t = runM (eval t) 0

    raise :: String -> M2 s String a
    raise str = M2(\s -> Izq str)

    modify :: (s -> s) -> M2 s e ()
    modify f = M2 (\s -> Der ((), f s))


    instance Functor (M2 s e) where
        fmap = liftM

    instance Applicative (M2 s e) where
        pure = return
        (<*>) = ap

    instance Monad (M2 s e) where
        return x = M2(\s -> Der (x, s))
        (M2 g) >>= f = M2 (\s ->   case (g s) of
                                        Izq e ->  Izq e
                                        Der (a, s') -> (runM(f a)) s)

--9)
    newtype M3 m a = Mk (m (Maybe a))

    instance Monad m => Functor (M3 m) where
        fmap = liftM

    instance Monad m => Applicative (M3 m) where
        pure = return
        (<*>) = ap

    instance Monad m => Monad (M3 m) where
        return x = Mk((return) (Just x))

    throw :: Monad m => M3 m a
    throw = Mk (return Nothing)

    data StInt a = St2 {runSt2 :: Int -> (a,Int)}
    type N a = M3 StInt a

    getM :: N Int
    getM = Mk (St2 (\n -> (Just n, n)))
    
  