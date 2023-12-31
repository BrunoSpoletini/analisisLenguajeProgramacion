module Eval2
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )
import Data.Either (Either(Right))

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }


-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\s -> Right (x :!: s))
  m >>= f = StateError (\s ->   let e = runStateError m s 
                                in case e of
                                    (Left err) -> Left err
                                    (Right (v :!: s')) -> runStateError (f v) s')

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
instance MonadError StateError where
  throw e = StateError(\s -> Left e)

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  lookfor v = StateError (\s -> lookfor' v s)
    where lookfor' v s = case M.lookup v s of
                          Nothing -> Left UndefVar
                          (Just x) -> Right (x :!: s)
  update v i = StateError (\s -> Right (() :!: (update' v i s))) 
    where update' = M.insert

-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval c =  case runStateError (stepCommStar c) initEnv of
            (Left err) -> Left err
            (Right (v :!: s')) -> Right s'

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
stepComm Skip = return Skip
stepComm (Let v e) =  do  i <- evalExp e
                          update v i
                          return Skip
stepComm (Seq Skip c2) = return c2
stepComm (Seq c1 c2) = do c <- stepComm c1
                          return (Seq c c2)
stepComm (IfThenElse e c1 c2) = do c <- evalExp e
                                   if c then
                                    return c1
                                   else
                                    return c2
stepComm (While e c) = return (Seq c (IfThenElse e (While e c) Skip))

-- Evalua una expresion
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
evalExp (Const x) = return x
evalExp (Var v) = lookfor v
evalExp (UMinus e) = do c <- evalExp e
                        return (-c)
evalExp (Plus e1 e2) = opera e1 e2 (+)
evalExp (Minus e1 e2) = opera e1 e2 (-)
evalExp (Times e1 e2) = opera e1 e2 (*)
evalExp (Div e1 e2) = divide e1 e2
evalExp BTrue = return True
evalExp BFalse = return False
evalExp (Lt e1 e2) = opera e1 e2 (<)
evalExp (Gt e1 e2) = opera e1 e2 (>)
evalExp (And b1 b2) = opera b1 b2 (&&)
evalExp (Or b1 b2) = opera b1 b2 (||)
evalExp (Not b1) = do c <- evalExp b1
                      return (not c)
evalExp (Eq e1 e2) = opera e1 e2 (==)
evalExp (NEq e1 e2) = opera e1 e2 (/=)
evalExp (ESeq a b) = do evalAss a
                        evalExp b

evalAss :: (MonadState m, MonadError m) => Exp a -> m ()
evalAss (EAssgn a b) = do v1 <- evalExp b
                          update a v1

opera :: (MonadState m, MonadError m) => Exp a -> Exp b -> (a -> b -> c) -> m c
opera e1 e2 f = do  v1 <- evalExp e1
                    v2 <- evalExp e2
                    return (f v1 v2)

divide :: (MonadState m, MonadError m) => Exp a -> Exp b -> (a -> b -> c) -> m c
divide e1 e2  = do  v1 <- evalExp e1
                    v2 <- evalExp e2
                    case divide' v1 v2 of
                      Left e -> throw e
                      Right x -> return x

divide' :: Either Error (Pair a Env) -> Either Error (Pair a Env) -> Either Error (Pair a Env)
divide' (Left e) _ = Left e
divide' _ (Left e) = Left e
divide' (Right x) (Right y) = if y == 0 then
                                  Left DivByZero
                                else
                                  Right (div v1 v2)
