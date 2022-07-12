{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}

module SystemF where

import qualified Data.List as List
import qualified Data.Bifunctor as Bf

import Data.Text (Text)

newtype Var = Var Text
  deriving (Eq)

newtype TVar = TVar Text
  deriving (Eq)

showVar :: Var -> Text
showVar (Var txt) = txt

showTVar :: TVar -> Text
showTVar (TVar txt) = txt

data Term
  = VarTerm Var
  | Lam Var Type Term
  | TLam TVar Term
  | App Term Term
  | TApp Term Type

data Type
  = VarType TVar
  | Arr Type Type
  | Forall TVar Type

type TermEnv = [(Var, Type)]

type TypeEnv = [TVar]

type Env = (TermEnv, TypeEnv)

addVar :: Var -> Type -> Env -> Env
addVar x t = Bf.first ((x, t) :)

addTVar :: TVar -> Env -> Env
addTVar a = Bf.second (a :)

lookupVar :: Var -> TermEnv -> Either Text Type
lookupVar x env =
  case List.lookup x env of
    Nothing -> Left (showVar x <> " is not defined")
    Just t -> Right t

lookupTVar :: TVar -> TypeEnv -> Either Text ()
lookupTVar a env
  | a `elem` env = Right ()
  | otherwise = Left (showTVar a <> " is not defined")

infer :: Env -> Term -> Either Text Type
infer env@(termEnv, typeEnv) = \case
  VarTerm x -> lookupVar x termEnv
  Lam x t e -> do
    validType typeEnv t
    t_e <- infer (addVar x t env) e
    return (Arr t t_e)
  TLam a e -> do
    t_e <- infer (addTVar a env) e
    return (Forall a t_e)
  App f e -> do
    t_f <- infer env f
    undefined

  _ -> undefined

validType :: TypeEnv -> Type -> Either Text ()
validType env = \case
  VarType a -> lookupTVar a env
  Arr t1 t2 -> validType env t1 >> validType env t2
  Forall a t -> validType (a : env) t
