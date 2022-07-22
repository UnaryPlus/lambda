{-# LANGUAGE LambdaCase, BlockArguments, TupleSections, OverloadedStrings, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies #-}

module HM (main) where

import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Control.Monad (join)
import qualified Data.Bool as B

import Text.Parsec (runParser, (<|>), choice, try, notFollowedBy, many, many1, chainl1, digit, string, spaces, eof)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Except (MonadError, ExceptT, throwError, runExceptT)
import Control.Monad.State (MonadState, State, evalState)
import qualified Control.Monad.State as State

import Util (prompt, Parser, symbol, parens, alphaNum, alpha, parensIf)

main :: IO ()
main = loop []

loop :: [(Var, Term)] -> IO ()
loop defs = do
  str <- prompt
  case runParser parseCommand () "input" str of
    Left err -> print err >> loop defs
    Right (Evaluate term) -> do
      let term' = addDefs defs term
      case runInfer term' of
        Left err -> IO.putStrLn err
        Right ty -> do
          IO.putStrLn (": " <> prettyType ty)
          IO.putStrLn (prettyTerm (runReduce term'))
      loop defs
    Right (Define x term) ->
      case runInfer (addDefs defs term) of
        Left err -> IO.putStrLn err >> loop defs
        Right ty -> do
          IO.putStrLn (": " <> prettyType ty)
          loop ((x, term) : defs)

addDefs :: [(Var, Term)] -> Term -> Term
addDefs defs term = foldl' (\e1 (x, e0) -> Let x e0 e1) term defs

runInfer :: Term -> Either Text Type
runInfer term = fst <$> evalState (runExceptT (infer Map.empty term)) 0

data Var
  = Var Text
  | VarId Text Int
  deriving (Eq, Ord)

newtype TVar = TVar Int
  deriving (Eq, Ord)

showVar :: Var -> Text
showVar = \case
  Var txt -> txt
  VarId txt i -> txt <> "_" <> Text.pack (show i)

showTVar :: TVar -> Text
showTVar (TVar i) = Text.pack (show i)

data Term
  = Const Const
  | VarTerm Var
  | Let Var Term Term
  | Lam Var Term
  | App Term Term
  deriving (Eq)

data Type
  = Bool
  | Int
  | Arr Type Type
  | VarType TVar

data Const
  = CTrue | CFalse
  | CInt Int
  | And | Or | Not | If
  | Add | Sub | Mul | Div | Mod
  | Eq | Lt | Gt
  deriving (Eq)

isInt :: Const -> Bool
isInt = \case
  CInt _ -> True
  _ -> False

reserved :: Map String Const
reserved = Map.fromList
  [ ("true", CTrue), ("false", CFalse)
  , ("and", And), ("or", Or), ("not", Not), ("if", If)
  , ("add", Add), ("sub", Sub), ("mul", Mul), ("div", Div), ("mod", Mod)
  , ("eq", Eq), ("lt", Lt), ("gt", Gt)
  ]

prettyConst :: Const -> Text
prettyConst = \case
  CTrue -> "true"; CFalse -> "false"
  CInt i -> Text.pack (show i)
  And -> "and"; Or -> "or"; Not -> "not"; If -> "if"
  Add -> "add"; Sub -> "sub"; Mul -> "mul"; Div -> "div"; Mod -> "mod"
  Eq -> "eq"; Lt -> "lt"; Gt -> "gt"

parseName :: Parser Term
parseName = do
  str <- (:) <$> alpha <*> many alphaNum
  spaces
  return case Map.lookup str reserved of
    Nothing -> VarTerm (Var (Text.pack str))
    Just k -> Const k

parseVar :: Parser Var
parseVar = do
  let res = choice (map string (Map.keys reserved))
  notFollowedBy (res >> notFollowedBy alphaNum)
  str <- (:) <$> alpha <*> many alphaNum
  spaces
  return (Var (Text.pack str))

data Command
  = Evaluate Term
  | Define Var Term

parseCommand :: Parser Command
parseCommand = do
  spaces
  cmd <- try parseDefine
    <|> (Evaluate <$> parseTerm)
  eof
  return cmd

parseDefine :: Parser Command
parseDefine = do
  x <- parseVar
  symbol '='
  Define x <$> parseTerm

parseTerm :: Parser Term
parseTerm = chainl1 parseFactor (return App)

parseFactor :: Parser Term
parseFactor =
  parseName <|>
  parseInt <|>
  parseLet <|>
  parseLam <|>
  parens parseTerm

parseInt :: Parser Term
parseInt = do
  i <- int
  spaces
  return (Const (CInt i))
  where
    nat = read <$> many1 digit
    int = (symbol '-' >> fmap negate nat) <|> nat

parseLet :: Parser Term
parseLet = do
  symbol '{'
  x <- parseVar
  symbol '='
  t <- parseTerm
  symbol '}'
  Let x t <$> parseTerm

parseLam :: Parser Term
parseLam = do
  symbol '\\' <|> symbol 'λ'
  x <- parseVar
  symbol '.'
  Lam x <$> parseTerm

type Env = Map Var Type
type Subst = Map TVar Type

applySubst :: Subst -> Type -> Type
applySubst s = \case
  Bool -> Bool
  Int -> Int
  Arr t0 t1 -> Arr (applySubst s t0) (applySubst s t1)
  VarType a -> fromMaybe (VarType a) (Map.lookup a s)

compose :: Subst -> Subst -> Subst
compose s1 s0 = s1 <> fmap (applySubst s1) s0

fresh :: MonadState Int m => m TVar
fresh = do
  i <- State.get
  State.modify (1+)
  return (TVar i)

constType :: MonadState Int m => Const -> m Type
constType k
  | k `elem` [CTrue, CFalse] = return Bool
  | isInt k = return Int
  | k `elem` [And, Or] = return (op Bool)
  | k == Not = return (Arr Bool Bool)
  | k == If = fresh >>= \a -> return (Arr Bool (op (VarType a)))
  | k `elem` [Add, Sub, Mul, Div, Mod] = return (op Int)
  | k `elem` [Eq, Lt, Gt] = return (Arr Int (Arr Int Bool))
  | otherwise = error "mistake in constType implementation"
  where op t = Arr t (Arr t t)

infer :: Env -> Term -> ExceptT Text (State Int) (Type, Subst)
infer env = \case
  Const k -> (, Map.empty) <$> constType k
  VarTerm x -> case Map.lookup x env of
    Nothing -> throwError (showVar x <> " is not defined")
    Just t -> return (t, Map.empty)
  Let x e0 e1 -> do
    (t0, s0) <- infer env e0
    let env' = Map.insert x t0 (fmap (applySubst s0) env)
    (t1, s1) <- infer env' e1
    return (t1, s1 `compose` s0)
  Lam x e -> do
    a <- VarType <$> fresh
    (t, s) <- infer (Map.insert x a env) e
    return (Arr (applySubst s a) t, s)
  App e0 e1 -> do
    (t0, s0) <- infer env e0
    (t1, s1) <- infer (fmap (applySubst s0) env) e1
    a <- VarType <$> fresh
    s2 <- unify (applySubst s1 t0) (Arr t1 a)
    return (applySubst s2 a, s2 `compose` s1 `compose` s0)

unify :: MonadError Text m => Type -> Type -> m Subst
unify = curry \case
  (VarType a, t) -> bind a t
  (t, VarType a) -> bind a t
  (Bool, Bool) -> return Map.empty
  (Int, Int) -> return Map.empty
  (Arr t0 t1, Arr u0 u1) -> do
    s0 <- unify t0 u0
    s1 <- unify (applySubst s0 t1) (applySubst s0 u1)
    return (s1 `compose` s0)
  (t0, t1) -> throwError ("cannot unify types:\n* "
    <> prettyType t0 <> "\n* " <> prettyType t1)

bind :: MonadError Text m => TVar -> Type -> m Subst
bind a t
  | a `freeIn` t = throwError ("cannot substitute "
      <> showTVar a <> ":\n* " <> prettyType t)
  | otherwise = return (Map.singleton a t)

class FreeVars v a | a -> v where
  freeIn :: v -> a -> Bool

instance FreeVars TVar Type where
  freeIn a = \case
    Bool -> False
    Int -> False
    Arr t0 t1 -> a `freeIn` t0 || a `freeIn` t1
    VarType b -> b == a

instance FreeVars Var Term where
  freeIn x = \case
    Const _ -> False
    VarTerm y -> y == x
    Let y e0 e1 -> x `freeIn` e0 || (y /= x && x `freeIn` e1)
    Lam y e -> y /= x && x `freeIn` e
    App e0 e1 -> x `freeIn` e0 || x `freeIn` e1

runReduce :: Term -> Term
runReduce e = evalState (reduce e) 0

reduce :: Term -> State Int Term
reduce = \case
  Const k -> return (Const k)
  VarTerm x -> return (VarTerm x)
  Let x e0 e1 -> reduce =<< subst x e0 e1
  Lam x e -> Lam x <$> reduce e
  App e0 e1 -> join (apply <$> reduce e0 <*> reduce e1)

apply :: Term -> Term -> State Int Term
apply f e1 = case f of
  App (Const And) e0 -> bool ((&&) <$> getBool e0 <*> getBool e1)
  App (Const Or) e0 -> bool ((||) <$> getBool e0 <*> getBool e1)
  Const Not -> bool (not <$> getBool e1)
  App (App (Const If) b) e0 -> term (B.bool e1 e0 <$> getBool b)
  App (Const Add) e0 -> int ((+) <$> getInt e0 <*> getInt e1)
  App (Const Sub) e0 -> int ((-) <$> getInt e0 <*> getInt e1)
  App (Const Mul) e0 -> int ((*) <$> getInt e0 <*> getInt e1)
  App (Const Div) e0 -> int (div <$> getInt e0 <*> getInt e1)
  App (Const Mod) e0 -> int (mod <$> getInt e0 <*> getInt e1)
  App (Const Eq) e0 -> bool ((==) <$> getInt e0 <*> getInt e1)
  App (Const Lt) e0 -> bool ((<) <$> getInt e0 <*> getInt e1)
  App (Const Gt) e0 -> bool ((>) <$> getInt e0 <*> getInt e1)
  Lam x e0 -> reduce =<< subst x e1 e0
  _ -> return (App f e1)
  where
    term = return . fromMaybe (App f e1)
    bool = return . maybe (App f e1) fromBool
    int = return . maybe (App f e1) fromInt

getBool :: Term -> Maybe Bool
getBool = \case
  Const CTrue -> Just True
  Const CFalse -> Just False
  _ -> Nothing

fromBool :: Bool -> Term
fromBool = \case
  True -> Const CTrue
  False -> Const CFalse

getInt :: Term -> Maybe Int
getInt = \case
  Const (CInt i) -> Just i
  _ -> Nothing

fromInt :: Int -> Term
fromInt = Const . CInt

freshVar :: Var -> State Int Var
freshVar x = do
  i <- State.get
  State.modify (1+)
  return case x of
    Var txt -> VarId txt i
    VarId txt _ -> VarId txt i

rename :: Var -> Var -> Term -> Term
rename x x' = \case
  Const k -> Const k
  VarTerm y -> VarTerm if y == x then x' else y
  Let y e0 e1 -> Let y (rename x x' e0)
    if y == x then e1 else rename x x' e1
  Lam y e0 -> Lam y if y == x then e0 else rename x x' e0
  App e0 e1 -> App (rename x x' e0) (rename x x' e1)

subst :: Var -> Term -> Term -> State Int Term
subst x e = \case
  Const k -> return (Const k)
  VarTerm y -> return if y == x then e else VarTerm y
  Let y e0 e1 -> subst x e (App (Lam y e1) e0)
  Lam y e0
    | y == x -> return (Lam y e0)
    | y `freeIn` e ->
        if x `freeIn` e0 then do
          y' <- freshVar y
          let e0' = rename y y' e0
          Lam y' <$> subst x e e0'
        else return (Lam y e0)
    | otherwise -> Lam y <$> subst x e e0
  App e0 e1 -> App <$> subst x e e0 <*> subst x e e1

data Context
  = Outer
  | AppLeft
  | AppRight
  deriving (Eq)

prettyTerm :: Term -> Text
prettyTerm = prettyTermP Outer

prettyTermP :: Context -> Term -> Text
prettyTermP ctx = \case
  Const k -> prettyConst k
  VarTerm x -> showVar x
  Let x e0 e1 ->
    parensIf (ctx == AppLeft || ctx == AppRight) $
    "{" <> showVar x <> " = " <> prettyTerm e0 <> "} " <> prettyTerm e1
  Lam x e ->
    parensIf (ctx == AppLeft || ctx == AppRight) $
    "λ" <> showVar x <> ". " <> prettyTerm e
  App e0 e1 ->
    parensIf (ctx == AppRight) $
    prettyTermP AppLeft e0 <> " " <> prettyTermP AppRight e1

prettyType :: Type -> Text
prettyType = prettyTypeP False

prettyTypeP :: Bool -> Type -> Text
prettyTypeP arrLeft = \case
  Bool -> "Bool"
  Int -> "Int"
  Arr t0 t1 ->
    parensIf arrLeft $
    prettyTypeP True t0 <> " → " <> prettyTypeP False t1
  VarType a -> showTVar a
