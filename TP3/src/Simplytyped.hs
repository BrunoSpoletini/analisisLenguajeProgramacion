module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n    ) = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (LApp t u  ) = conversion' b t :@: conversion' b u
conversion' b (LAbs n t u) = Lam t (conversion' (n : b) u)
conversion' b (LLet n t1 t2) = Let (conversion' b t1) (conversion'  (n:b) t2)
conversion' b (LUnit)      = Unit 
conversion' b (LPair u v) = Pair  (conversion' b u) (conversion' b v)
conversion' b (LFst s) = Fst (conversion' b s)
conversion' b (LSnd s) = Snd (conversion' b s)
conversion' b (LZero)  = Zero
conversion' b (LSuc s) = Suc (conversion' b s)
conversion' b (LRec t1 t2 t3) = Rec (conversion' b t1)  (conversion' b t2) (conversion' b t3)

-----------------------
--- eval
-----------------------

sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)
sub i t (Let t1 t2)           = Let (sub i t t1) (sub (i + 1) t t2)
sub i t (Unit)                = Unit
sub i t (Pair u v)            = Pair (sub i t u) (sub i t v)
sub i t (Fst s)               = Fst (sub i t s)
sub i t (Snd s)               = Snd (sub i t s)
sub i t (Zero)                = Zero
sub i t (Suc s)               = Suc (sub i t s)
sub i t (Rec t1 t2 t3)        = Rec (sub i t t1)  (sub i t t2) (sub i t t3)
-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _             ) = error "variable ligada inesperada en eval"
eval e (Free  n             ) = fst $ fromJust $ lookup n e
eval _ (Lam      t   u      ) = VLam t u
eval e (Lam _ u  :@: Lam s v) = eval e (sub 0 (Lam s v) u)
eval e (Lam t u1 :@: u2) = let v2 = eval e u2 in eval e (sub 0 (quote v2) u1)
eval e (u        :@: v      ) = case eval e u of
  VLam t u' -> eval e (Lam t u' :@: v)
  _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Let t1 t2) = eval e (sub 0 (quote (eval e t1)) t2)
eval e (Unit) = VUnit
eval e (Fst t) = case eval e t of
                 VPair t1 t2 -> t1
                 _           -> error "Error de tipo, solo se puede aplicar fst a una tupla"
eval e (Snd t) = case eval e t of
                 VPair t1 t2 -> t2
                 _           -> error "Error de tipo, solo se puede aplicar snd a una tupla"
eval e (Pair u v) = VPair (eval e u) (eval e v)
eval e (Zero) = VNum NZero
eval e (Suc t) = case eval e t of
                 VNum x -> VNum (NSuc x)
                 _ -> error "Error de tipo, no natural"
eval e (Rec t1 t2 t3) = case eval e t3 of
                        VNum NZero -> eval e t1
                        VNum (NSuc x) -> eval e (t2 :@: Rec t1 t2 t' :@: t')
                                         where t' = quote (VNum x) 
                        _ -> error "Error de tipo, no es un natural"            
                 
-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t f) = Lam t f
quote (VUnit)    = Unit
quote (VPair u v) = Pair (quote u ) (quote v)
quote (VNum NZero) = Zero
quote (VNum (NSuc x)) = Suc (quote ( VNum x))

----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

isSameType :: Type -> Type -> Either String Type
isSameType tt tu = if tt == tu then ret tt else matchError tt tu

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free  n) = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu
infer' c e (Let t1 t2) =  infer' c e t1 >>= \tt1 -> infer' (tt1 : c) e t2
infer' c e (Unit)    = ret UnitT
infer' c e (Pair u v) = infer' c e u >>= \tu ->
                        infer' c e v >>= \tv ->
                        ret $ PairT tu tv
infer' c e (Fst s) = infer' c e s >>= \t ->
                     case t of PairT x y -> ret x
                               _         -> err "Fst está tomando algo que no es una tupla"
infer' c e (Snd s) = infer' c e s >>= \t ->
                     case t of PairT x y -> ret y
                               _         -> err "Snd está tomando algo que no es una tupla"
infer' c e (Zero) = ret NatT
infer' c e (Suc s) = infer' c e s >>= \s' -> if s' == NatT then ret NatT else matchError NatT s'
infer' c e (Rec t1 t2 t3) = 
    infer' c e t1 >>= \ts ->
    infer' c e t2 >>= \tt ->
    infer' c e t3 >>= \tu ->
    isSameType tt (FunT ts (FunT NatT ts)) >>= \_ ->
    isSameType tu NatT >>= \_ ->
    ret ts
----------------------------------
