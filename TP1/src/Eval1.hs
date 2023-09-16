module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado nulo
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Int
lookfor v s = s M.! v

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update = M.insert 

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip s = Skip :!: s
stepComm (Let v i) s = let i' :!: s' = (evalExp i s)
                       in Skip :!: update v i' s'
stepComm (Seq Skip c2) s = c2 :!: s
stepComm (Seq c1 c2) s = let c1':!: s' =  stepComm c1 s 
                         in (Seq c1' c2) :!: s'
stepComm (IfThenElse b c1 c2) s = let b' :!: s' = (evalExp b s)
                                  in if b' then c1 :!: s' else c2 :!: s'
stepComm (Repeat c b) s = (Seq c (IfThenElse b Skip (Repeat c b))) :!: s

-- Evalua una expresion
evalExp :: Exp a -> State -> Pair a State
evalExp (Const x) s = x :!: s
evalExp (Var v) s = (lookfor v s) :!: s
evalExp (UMinus e) s = let v :!: s' = evalExp e s 
                       in -v :!: s'
evalExp (Plus e1 e2) s = opera e1 e2 s (+)
evalExp (Minus e1 e2) s = opera e1 e2 s (-)
evalExp (Times e1 e2) s = opera e1 e2 s (*)
evalExp (Div e1 e2) s = opera e1 e2 s div
evalExp BTrue s = True :!: s
evalExp BFalse s = False :!: s
evalExp (Lt e1 e2) s = opera e1 e2 s (<)
evalExp (Gt e1 e2) s = opera e1 e2 s (>)
evalExp (And b1 b2) s = opera b1 b2 s (&&)
evalExp (Or b1 b2) s = opera b1 b2 s (||)
evalExp (Not b1) s = let x :!: s' = evalExp b1 s 
                     in (not x) :!: s'
evalExp (Eq e1 e2) s = opera e1 e2 s (==)
evalExp (NEq e1 e2) s = opera e1 e2 s (/=)
evalExp (ESeq a b) s = let s' = evalAss a s
                       in evalExp b s'

evalAss :: Exp a -> State -> State
evalAss (EAssgn a b) s = let res :!: s' = evalExp b s
                         in update a res s'

opera:: Exp a -> Exp b -> State -> (a -> b -> c) -> Pair c State
opera e1 e2 s f = let e1' :!: s' = (evalExp e1 s)
                      e2' :!: s'' = (evalExp e2 s') 
                  in (f e1' e2') :!: s''
