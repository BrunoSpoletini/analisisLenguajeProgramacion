module Eval2
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple
import Data.Either (Either(Right))

-- Estados
type State = M.Map Variable Int

-- Estado nulo
initState :: State
initState = M.empty

newtype StateError a = StateError {runStateError :: Env -> Either Error (a, Env)}

instance Monad StateError where
  return x = StateError(\s -> Right(x :!: s))
  m >>= f = 

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case s M.!? v of
              Nothing -> Left UndefVar
              Just a -> Right a 

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalua un paso de un comando en un estado dado
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip s = Right (Skip :!: s)
stepComm (Let v i) s = case (evalExp i s) of
                            Left l -> Left l
                            Right (n:!: s') -> Right (Skip :!: update v n s')
stepComm (Seq Skip c2) s = Right (c2 :!: s)
stepComm (Seq c1 c2) s = case (stepComm c1 s) of
                              Left l -> Left l
                              Right (c1':!: s') -> Right ((Seq c1' c2) :!: s')
stepComm (IfThenElse b c1 c2) s = case (evalExp b s) of
                                       Left l -> Left l
                                       Right (r:!: s') -> if r then Right(c1 :!: s') else Right(c2 :!: s')
stepComm (Repeat c b) s = case (evalExp b s) of
                              Left l -> Left l
                              Right (r:!: s') -> Right((Seq c (IfThenElse b Skip (Repeat c b))) :!: s) 


-- Evalua una expresion
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const x) s = Right (x :!: s)
evalExp (Var v) s =case (lookfor v s)  of 
                         Left l -> Left l
                         Right r -> Right (r :!: s)
evalExp (UMinus e) s = case (evalExp e s) of
                            Left l -> Left l
                            Right (r :!: s')  -> Right  (-r :!: s')
evalExp (Plus e1 e2) s = opera e1 e2 s (+)
evalExp (Minus e1 e2) s = opera e1 e2 s (-)
evalExp (Times e1 e2) s = opera e1 e2 s (*)
evalExp (Div e1 e2) s = case (evalExp e1 s) of
                             Left l -> Left l
                             Right (n :!: s') -> case (evalExp e2 s') of
                                                      Left l -> Left l
                                                      Right (n2 :!: s'') -> if n2 == 0 then Left DivByZero else Right ((div n n2) :!: s'')
evalExp BTrue s = Right (True :!: s)
evalExp BFalse s = Right (False :!: s)
evalExp (Lt e1 e2) s = opera e1 e2 s (<)
evalExp (Gt e1 e2) s = opera e1 e2 s (>)
evalExp (And b1 b2) s = opera b1 b2 s (&&)
evalExp (Or b1 b2) s = opera b1 b2 s (||)
evalExp (Not b1) s = case (evalExp b1 s) of
                            Left l -> Left l
                            Right (r :!: s')  -> Right  ((not r) :!: s')
evalExp (Eq e1 e2) s = opera e1 e2 s (==)
evalExp (NEq e1 e2) s = opera e1 e2 s (/=)
evalExp (ESeq a b) s = case (evalAss a s) of
                             Left l -> Left l
                             Right s' -> evalExp b s'

evalAss :: Exp a -> State -> Either Error State
evalAss (EAssgn a b) s = case (evalExp b s) of
                               Left l -> Left l
                               Right (res :!: s') -> Right (update a res s')


opera:: Exp a -> Exp b -> State -> (a -> b -> c) -> Either Error (Pair c State)
opera e1 e2 s f = case (evalExp e1 s) of
                        Left l -> Left l
                        Right (e1' :!: s') -> case (evalExp e2 s') of
                                                    Left l -> Left l
                                                    Right (e2' :!: s'') -> Right((f e1' e2') :!: s'')
                    
