module TypeInf where

import Data.Maybe

data Expr = Number Int |
            Boolean Bool |
            Id String  |
            Prim String |
            Cond Expr Expr Expr |
            App Expr Expr |
            Fun String Expr
          deriving (Eq, Show)

data Type = TInt |
            TBool |
            TFun Type Type |
            TVar String |
            TErr
          deriving (Eq, Show)

showT :: Type -> String
showT TInt
  = "Int"
showT TBool
  = "Bool"
showT (TFun t t')
  = "(" ++ showT t ++ " -> " ++ showT t' ++ ")"
showT (TVar a)
  = a
showT TErr
  = "Type error"

type TypeTable = [(String, Type)]

type TEnv
  = TypeTable    -- i.e. [(String, Type)]

type Sub
  = TypeTable    -- i.e. [(String, Type)]

-- Built-in function types...
primTypes :: TypeTable
primTypes
  = [("+", TFun TInt (TFun TInt TInt)),
     (">", TFun TInt (TFun TInt TBool)),
     ("==", TFun TInt (TFun TInt TBool)),
     ("not", TFun TBool TBool)]

------------------------------------------------------
-- PART I

-- Pre: The search item is in the table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x xys
  = fromJust (lookup x xys)

tryToLookUp :: Eq a => a -> b -> [(a, b)] -> b
tryToLookUp x y xys
  = fromMaybe y (lookup x xys)

-- Pre: The given value is in the table
reverseLookUp :: Eq b => b -> [(a, b)] -> [a]
reverseLookUp y
  = foldl (\xs (x', y') -> if y' == y then x' : xs else xs) []

occurs :: String -> Type -> Bool
occurs str (TFun t1 t2)
  = occurs str t1 || occurs str t2
occurs str (TVar str')
  | str' == str = True
occurs _ _
  = False

------------------------------------------------------
-- PART II

-- Pre: There are no user-defined functions (constructor Fun)
-- Pre: All type variables in the expression have a binding in the given
--      type environment
inferType :: Expr -> TEnv -> Type
inferType (Number _) _
  = TInt
inferType (Boolean _) _
  = TBool
inferType (Id str) tenv
  = lookUp str tenv
inferType (Prim str) _
  = lookUp str primTypes
inferType (Cond eP eQ eR) tenv
  | tP /= TBool || tQ /= tR = TErr
  | otherwise               = tQ
  where
    tP = inferType eP tenv
    tQ = inferType eQ tenv
    tR = inferType eR tenv
inferType (App f e) tenv
  = inferApp tF tE
  where
    tF = inferType f tenv
    tE = inferType e tenv

inferApp :: Type -> Type -> Type
inferApp (TFun t1 t2) tE
  | t1 == tE = t2
inferApp _ _
  = TErr

------------------------------------------------------
-- PART III

applySub :: Sub -> Type -> Type
applySub sub (TFun t1 t2)
  = TFun (applySub sub t1) (applySub sub t2)
applySub sub (TVar str)
  = tryToLookUp str (TVar str) sub
applySub _ t
  = t

unify :: Type -> Type -> Maybe Sub
unify t t'
  = unifyPairs [(t, t')] []

unifyPairs :: [(Type, Type)] -> Sub -> Maybe Sub
unifyPairs [] sub
  = Just sub
unifyPairs ((TInt, TInt) : tts) sub
  = unifyPairs tts sub
unifyPairs ((TBool, TBool) : tts) sub
  = unifyPairs tts sub
unifyPairs ((TVar v1, TVar v2) : tts) sub
  | v1 == v2 = unifyPairs tts sub
unifyPairs ((TVar v, t) : tts) sub
  | occurs v t = Nothing
  | otherwise  = unifyPairs (map update tts) ((v, t) : sub)
  where
    update (t1, t2) = (applySub [(v, t)] t1, applySub [(v, t)] t2)
unifyPairs ((t, TVar v) : tts) sub
  = unifyPairs ((TVar v, t) : tts) sub
unifyPairs ((TFun t1 t2, TFun t1' t2') : tts) sub
  = unifyPairs ([(t1, t1'), (t2, t2')] ++ tts) sub
unifyPairs _ _
  = Nothing

------------------------------------------------------
-- PART IV

updateTEnv :: TEnv -> Sub -> TEnv
updateTEnv tenv tsub
  = map modify tenv
  where
    modify (v, t) = (v, applySub tsub t)

combine :: Sub -> Sub -> Sub
combine sNew sOld
  = sNew ++ updateTEnv sOld sNew

-- In combineSubs [s1, s2,..., sn], s1 should be the *most recent* substitution
-- and will be applied *last*
combineSubs :: [Sub] -> Sub
combineSubs
  = foldr1 combine

inferPolyType :: Expr -> Type
inferPolyType e
  = if isError t then TErr else t
  where
    t = (\(s, t, ss) -> t) (inferPolyType' e [] (map (('a' :) . show) [1..]))

inferPolyType' :: Expr -> TEnv -> [String] -> (Sub, Type, [String])
inferPolyType' (Number _) _ ss
  = ([], TInt, ss)
inferPolyType' (Boolean _) _ ss
  = ([], TBool, ss)
inferPolyType' (Prim str) _ ss
  = ([], lookUp str primTypes, ss)
inferPolyType' (Id str) tenv ss
  = ([], lookUp str tenv, ss)
inferPolyType' (Fun x e) tenv (s : ss)
  = (sub', TFun (applySub sub' (TVar s)) te, ss')
  where
    (sub, te, ss') = inferPolyType' e ((x, TVar s) : tenv) ss
    sub'           = (x, TVar s) : sub
inferPolyType' (App f e) tenv (s : ss)
  | isNothing maybeSub = ([], TErr, ss'')
  | otherwise          = (subs, applySub sub'' (TVar s), ss'')
  where
    (sub, tf, ss')   = inferPolyType' f tenv ss
    (sub', te, ss'') = inferPolyType' e (updateTEnv tenv sub) ss'
    tf'              = applySub (sub ++ sub') (TFun te (TVar s))
    maybeSub         = unify (applySub sub' tf) tf'
    sub''            = fromJust maybeSub
    subs             = combineSubs [sub'', sub', sub]

isError :: Type -> Bool
isError TErr
  = True
isError (TFun t1 t2)
  = isError t1 || isError t2
isError _
  = False

------------------------------------------------------
-- Monomorphic type inference test cases from Table 1...

env :: TEnv
env = [("x",TInt),("y",TInt),("b",TBool),("c",TBool)]

ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8 :: Expr
type1, type2, type3, type4, type5, type6, type7, type8 :: Type

ex1 = Number 9
type1 = TInt

ex2 = Boolean False
type2 = TBool

ex3 = Prim "not"
type3 =  TFun TBool TBool

ex4 = App (Prim "not") (Boolean True)
type4 = TBool

ex5 = App (Prim ">") (Number 0)
type5 = TFun TInt TBool

ex6 = App (App (Prim "+") (Boolean True)) (Number 5)
type6 = TErr

ex7 = Cond (Boolean True) (Boolean False) (Id "c")
type7 = TBool

ex8 = Cond (App (Prim "==") (Number 4)) (Id "b") (Id "c")
type8 = TErr

------------------------------------------------------
-- Unification test cases from Table 2...

u1a, u1b, u2a, u2b, u3a, u3b, u4a, u4b, u5a, u5b, u6a, u6b :: Type
sub1, sub2, sub3, sub4, sub5, sub6 :: Maybe Sub

u1a = TFun (TVar "a") TInt
u1b = TVar "b"
sub1 = Just [("b",TFun (TVar "a") TInt)]

u2a = TFun TBool TBool
u2b = TFun TBool TBool
sub2 = Just []

u3a = TFun (TVar "a") TInt
u3b = TFun TBool TInt
sub3 = Just [("a",TBool)]

u4a = TBool
u4b = TFun TInt TBool
sub4 = Nothing

u5a = TFun (TVar "a") TInt
u5b = TFun TBool (TVar "b")
sub5 = Just [("b",TInt),("a",TBool)]

u6a = TFun (TVar "a") (TVar "a")
u6b = TVar "a"
sub6 = Nothing

------------------------------------------------------
-- Polymorphic type inference test cases from Table 3...

ex9, ex10, ex11, ex12, ex13, ex14, ex15, ex16 :: Expr
type9, type10, type11, type12, type13, type14, type15, type16 :: Type

ex9 = Fun "x" (Boolean True)
type9 = TFun (TVar "a1") TBool

ex10 = Fun "x" (Id "x")
type10 = TFun (TVar "a1") (TVar "a1")

ex11 = Fun "x" (App (Prim "not") (Id "x"))
type11 = TFun TBool TBool

ex12 = Fun "x" (Fun "y" (App (Id "y") (Id "x")))
type12 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TVar "a3")) (TVar "a3"))

ex13 = Fun "x" (Fun "y" (App (App (Id "y") (Id "x")) (Number 7)))
type13 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TFun TInt (TVar "a3")))
              (TVar "a3"))

ex14 = Fun "x" (Fun "y" (App (Id "x") (Prim "+")))
type14 = TFun (TFun (TFun TInt (TFun TInt TInt)) (TVar "a3"))
              (TFun (TVar "a2") (TVar "a3"))

ex15 = Fun "x" (Fun "y" (App (App (Prim "+") (Id "y")) (Id "x")))
type15 = TFun TInt (TFun TInt TInt)

ex16 = Fun "x" (Fun "y" (App (App (Prim "+") (Id "y")) (App (Prim "not") (Id "x"))))
type16 = TErr
