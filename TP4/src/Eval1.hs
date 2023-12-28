module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
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
stepComm (Repeat c e) = return (Seq c (IfThenElse e Skip (Repeat c b)))

-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
evalExp (Const x) = return x
evalExp (Var v) = lookfor v
evalExp (UMinus e) = do c <- evalExp e
                        return (-c)
evalExp (Plus e1 e2) = opera e1 e2 (+)
evalExp (Minus e1 e2) = opera e1 e2 (-)
evalExp (Times e1 e2) = opera e1 e2 (*)
evalExp (Div e1 e2) = opera e1 e2 div
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

evalAss :: MonadState m => Exp a -> m ()
evalAss (EAssgn a b) = do v1 <- evalExp b
                          update a v1

opera :: MonadState m => Exp a -> Exp b -> (a -> b -> c) -> m c
opera e1 e2 f = do  v1 <- evalExp e1
                    v2 <- evalExp e2
                    return (f v1 v2)

