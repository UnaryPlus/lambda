{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module CoC where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)

newtype Name = Name { getName :: Text }
  deriving (Eq, Ord)

data Term
  = Type
  | Prop
  | Var Name
  | Lam Name Term Term
  | Prod Name Term Term
  | App Term Term

type Env = Map Name Term

verifyKind :: Term -> Either Text ()
verifyKind = \case
  Type -> Right ()
  Prop -> Right ()
  _ -> Left "was expecting Type or Prop"

matchProd :: Term -> Either Text (Name, Term, Term)
matchProd = \case
  Prod n x1 x2 -> Right (n, x1, x2)
  _ -> Left "was expecting a product type"

verifyEquiv :: Term -> Term -> Either Text ()
verifyEquiv = undefined

infer :: Env -> Term -> Either Text Term
infer env = \case
  Type -> Left "tried to infer type of Type"
  Prop -> Right Type
  Var n -> case Map.lookup n env of
    Nothing -> Left (getName n <> " is not defined")
    Just t -> Right t
  Lam n x1 x2 -> do
    t1 <- infer env x1
    verifyKind t1
    t2 <- infer (Map.insert n x1 env) x2
    Right (Prod n x1 t2)
  Prod n x1 x2 -> do
    t1 <- infer env x1
    verifyKind t1
    t2 <- infer (Map.insert n x1 env) x2
    verifyKind t2
    Right t2
  App x1 x2 -> do
    t1 <- infer env x1
    (n, t2, t3) <- matchProd t1
    t4 <- infer env x2
    verifyEquiv t4 t2
    Right (subst n x2 t3)

subst :: Name -> Term -> Term -> Term
subst = undefined

{-
verifyKind :: Term -> Either String ()
verifyKind = \case
  Type -> Right ()
  Prop -> Right ()
  _ -> Left "was expecting Type or Prop"

matchProd :: Term -> Either String (Name, Term, Term)
matchProd = \case
  Prod n x1 x2 -> Right (n, x1, x2)
  _ -> Left "was expecting a product type"

reduce :: Term -> Term
reduce = \case
  Type -> Type
  Prop -> Prop
  Var n -> Var n
  App x1 x2 -> App (reduce x1) (reduce x2)

verifyEquiv :: Term -> Term -> Either String ()
verifyEquiv = undefined --IT'S COMPLICATED

subst :: Name -> Term -> Term -> Term
subst n x = \case
  Type -> Type
  Prop -> Prop
  Var n1
    | n1 == n -> x
    | otherwise -> Var n1
  App x1 x2 -> App (subst n x x1) (subst n x x2)
  Lam n1 x1 x2 ->
    let x3 = if n1 == n then x2 else subst n x x2
    in Lam n1 (subst n x x1) x3
  Prod n1 x1 x2 ->
    let x3 = if n1 == n then x2 else subst n x x2
    in Prod n1 (subst n x x1) x3

infer :: Env -> Term -> Either String Term
infer env = \case
  Type -> Left "tried to infer type of Type"
  Prop -> Right Type
  Var n -> case Map.lookup n env of
    Nothing -> Left (getName n ++ " is not defined")
    Just t -> Right t
  App x1 x2 -> do
    t1 <- infer env x1
    (n, t2, t3) <- matchProd t1
    t4 <- infer env x2
    verifyEquiv t4 t2
    Right (subst n x2 t3)
  Lam n x1 x2 -> do
    t1 <- infer env x1
    verifyKind t1
    t2 <- infer (Map.insert n x1 env) x2
    Right (Prod n x1 t2)
  Prod n x1 x2 -> do
    t1 <- infer env x1
    verifyKind t1
    t2 <- infer (Map.insert n x1 env) x2
    verifyKind t2
    Right t2
-}
