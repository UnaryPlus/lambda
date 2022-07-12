{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}

module SystemF (main) where

import Data.Functor.Identity (runIdentity)
import qualified Data.List as List
import qualified Data.Bifunctor as Bf
import qualified Control.Monad as Monad

import Text.Parsec (runParser, (<|>), try, many1, chainr1, char, spaces, eof)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO

import Control.Monad.Trans (lift)
import Control.Monad.State (State, StateT, mapStateT, evalStateT, evalState)
import qualified Control.Monad.State as State

import Util (prompt, Parser, symbol, parens, squares, alphaNum, parensIf)
import qualified Lambda

main :: IO ()
main = loop []

loop :: [Def] -> IO ()
loop defs = do
  str <- prompt
  case runParser parseCommand () "input" str of
    Left err -> print err
    Right (Evaluate term) -> do
      case runInfer defs term of
        Left err -> IO.putStrLn err
        Right ty -> do
          IO.putStrLn ("type: " <> pretty ty)
          IO.putStrLn $ Lambda.pretty $ Lambda.runReduce (compile term)
      loop defs
    Right (DefTerm x term) ->
      case runInfer defs term of
        Left err -> IO.putStrLn err >> loop defs
        Right ty -> do
          IO.putStrLn ("type: " <> pretty ty)
          loop (Term x term : defs)
    Right (DefType a ty) ->
      case runValidType defs ty of
        Left err -> IO.putStrLn err >> loop defs
        Right () -> loop (Type a ty : defs)

inject :: (Monad m) => State s a -> StateT s m a
inject = mapStateT (return . runIdentity)

runInfer :: [Def] -> Term -> Either Text Type
runInfer defs e = flip evalStateT 0 do
  e' <- inject (addDefs Let e defs)
  infer ([], []) e'

runValidType :: [Def] -> Type -> Either Text ()
runValidType defs t = let
  t' = evalState (addDefs (\_ _ -> id) t defs) 0
  in validType [] t'

addDefs :: Subst a => (Var -> Term -> a -> a) -> a -> [Def] -> State Int a
addDefs = Monad.foldM . addDef

addDef :: Subst a => (Var -> Term -> a -> a) -> a -> Def -> State Int a
addDef let_ e = \case
  Term x e_x -> return (let_ x e_x e)
  Type a t -> subst a t e

newtype Var = Var { showVar :: Text }
  deriving (Eq)

data TVar
  = TVar Text
  | TVarId Text Int
  deriving (Eq)

showTVar :: TVar -> Text
showTVar = \case
  TVar txt -> txt
  TVarId txt i -> txt <> "_" <> Text.pack (show i)

data Term
  = VarTerm Var
  | Let Var Term Term
  | Lam Var Type Term
  | TLam TVar Term
  | App Term Term
  | TApp Term Type

data Type
  = VarType TVar
  | Arr Type Type
  | Forall TVar Type

data Command
  = DefTerm Var Term
  | DefType TVar Type
  | Evaluate Term

data Def
  = Term Var Term
  | Type TVar Type

parseCommand :: Parser Command
parseCommand = do
  spaces
  cmd <- try parseDefine
    <|> (Evaluate <$> parseTerm)
  eof
  return cmd

parseDefine :: Parser Command
parseDefine = parseName >>= \txt ->
  (symbol '=' >> DefTerm (Var txt) <$> parseTerm) <|>
  (symbol '~' >> DefType (TVar txt) <$> parseType)

parseName :: Parser Text
parseName = do
  str <- many1 alphaNum
  spaces
  return (Text.pack str)

parseTerm :: Parser Term
parseTerm = parseApp =<< parseFactor

parseApp :: Term -> Parser Term
parseApp f = do
    arg <- parseArg
    parseApp case arg of
      Left e -> App f e
      Right t -> TApp f t
  <|> return f

parseArg :: Parser (Either Term Type)
parseArg =
  (Left <$> parseFactor) <|>
  (Right <$> squares parseType)

parseFactor :: Parser Term
parseFactor =
  (VarTerm . Var <$> parseName)
  <|> parseLam
  <|> parens parseTerm

parseLam :: Parser Term
parseLam = do
  symbol '\\' <|> symbol 'λ'
  txt <- parseName
  (symbol '.' >> TLam (TVar txt) <$> parseTerm)
    <|> do
    symbol ':'
    t <- parseType
    symbol '.'
    Lam (Var txt) t <$> parseTerm

parseType :: Parser Type
parseType =
  chainr1 parseTFactor (arrow >> return Arr)
  where arrow = (char '-' >> symbol '>') <|> symbol '→'

parseTFactor :: Parser Type
parseTFactor =
  (VarType . TVar <$> parseName)
  <|> parseForall
  <|> parens parseType

parseForall :: Parser Type
parseForall = do
  symbol '?' <|> symbol '∀'
  a <- TVar <$> parseName
  symbol '.'
  Forall a <$> parseType

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

matchArr :: Type -> Either Text (Type, Type)
matchArr = \case
  Arr t1 t2 -> Right (t1, t2)
  _ -> Left "was expecting function type"

matchForall :: Type -> Either Text (TVar, Type)
matchForall = \case
  Forall a t -> Right (a, t)
  _ -> Left "was expecting forall type"

verifyEquiv :: Type -> Type -> Either Text ()
verifyEquiv t u
  | equivalent 0 t u = Right ()
  | otherwise = Left "could not match types"

equivalent :: Int -> Type -> Type -> Bool
equivalent i = curry \case
  (VarType a, VarType b) -> a == b
  (Arr t1 t2, Arr u1 u2) -> equivalent i t1 u1 && equivalent i t2 u2
  (Forall a t, Forall b u) ->
    let temp = TVar ("#" <> Text.pack (show i))
    in equivalent (i+1) (rename a temp t) (rename b temp u)
  _ -> False

fresh :: TVar -> State Int TVar
fresh a = do
  i <- State.get
  State.modify (1+)
  return case a of
    TVar txt -> TVarId txt i
    TVarId txt _ -> TVarId txt i

class Subst a where
  freeIn :: TVar -> a -> Bool
  rename :: TVar -> TVar -> a -> a
  subst :: TVar -> Type -> a -> State Int a

instance Subst Type where
  freeIn a = \case
    VarType b -> b == a
    Arr t1 t2 -> a `freeIn` t1 || a `freeIn` t2
    Forall b t -> a /= b && a `freeIn` t

  rename a a' = \case
    VarType b
      | b == a -> VarType a'
      | otherwise -> VarType b
    Arr t1 t2 -> Arr (rename a a' t1) (rename a a' t2)
    Forall b t
      | b == a -> Forall b t
      | otherwise -> Forall b (rename a a' t)

  subst a t = \case
    VarType b
      | b == a -> return t
      | otherwise -> return (VarType b)
    Arr t1 t2 -> Arr <$> subst a t t1 <*> subst a t t2
    Forall b u -> substAbst a t Forall (b, u)

instance Subst Term where
  freeIn a = \case
    VarTerm _ -> False
    Let _ e_x e -> a `freeIn` e_x || a `freeIn` e
    Lam _ t e -> a `freeIn` t || a `freeIn` e
    TLam b e -> a /= b && a `freeIn` e
    App f e -> a `freeIn` f || a `freeIn` e
    TApp f t -> a `freeIn` f || a `freeIn` t

  rename a a' = \case
    VarTerm x -> VarTerm x
    Let x e_x e -> Let x (rename a a' e_x) (rename a a' e)
    Lam x t e -> Lam x (rename a a' t) (rename a a' e)
    TLam b e
      | b == a -> TLam b e
      | otherwise -> TLam b (rename a a' e)
    App f e -> App (rename a a' f) (rename a a' e)
    TApp f t -> TApp (rename a a' f) (rename a a' t)

  subst a t = \case
    VarTerm x -> return (VarTerm x)
    Let x e_x e -> Let x <$> subst a t e_x <*> subst a t e
    Lam x t_x e -> Lam x <$> subst a t t_x <*> subst a t e
    TLam b e -> substAbst a t TLam (b, e)
    App f e -> App <$> subst a t f <*> subst a t e
    TApp f u -> TApp <$> subst a t f <*> subst a t u

substAbst :: Subst a => TVar -> Type -> (TVar -> a -> a) -> (TVar, a) -> State Int a
substAbst a t forall (b, u)
  | b == a = return (forall b u)
  | b `freeIn` t =
      if a `freeIn` u then do
        b' <- fresh b
        let u' = rename b b' u
        forall b' <$> subst a t u'
      else return (forall b u)
  | otherwise = forall b <$> subst a t u

infer :: Env -> Term -> StateT Int (Either Text) Type
infer env@(termEnv, typeEnv) = \case
  VarTerm x -> lift (lookupVar x termEnv)
  Let x e_x e -> do
    t_x <- infer env e_x
    infer (addVar x t_x env) e
  Lam x t e -> do
    lift (validType typeEnv t)
    t_e <- infer (addVar x t env) e
    return (Arr t t_e)
  TLam a e -> do
    t_e <- infer (addTVar a env) e
    return (Forall a t_e)
  App f e -> do
    (t1, t2) <- lift . matchArr =<< infer env f
    t_e <- infer env e
    lift (verifyEquiv t1 t_e)
    return t2
  TApp f t -> do
    (a, u) <- lift . matchForall =<< infer env f
    lift (validType typeEnv t)
    inject (subst a t u)

validType :: TypeEnv -> Type -> Either Text ()
validType env = \case
  VarType a -> lookupTVar a env
  Arr t1 t2 -> validType env t1 >> validType env t2
  Forall a t -> validType (a : env) t

pretty :: Type -> Text
pretty = prettyP False

prettyP :: Bool -> Type -> Text
prettyP arrLeft = \case
  VarType a -> showTVar a
  Arr t1 t2 -> parensIf arrLeft (prettyP True t1 <> " → " <> prettyP False t2)
  Forall a t -> parensIf arrLeft ("∀" <> showTVar a <> ". " <> prettyP False t)

compile :: Term -> Lambda.Term
compile = \case
  VarTerm x -> Lambda.Var (compileVar x)
  Let x e_x e -> Lambda.App (Lambda.Lam (compileVar x) (compile e)) (compile e_x)
  Lam x _ e -> Lambda.Lam (compileVar x) (compile e)
  TLam _ e -> compile e
  App f e -> Lambda.App (compile f) (compile e)
  TApp e _ -> compile e
  where compileVar (Var txt) = Lambda.Name txt
