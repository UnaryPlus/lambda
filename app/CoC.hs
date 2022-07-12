{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}

module CoC (main) where

import qualified Data.List as List
import qualified Control.Monad as Monad

import Text.Parsec (runParser, (<|>), try, many, chainl1, satisfy, spaces, eof)

import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Control.Monad.State (State, evalState)
import qualified Control.Monad.State as State

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO

import Util (prompt, Parser, symbol, parens, squares, isAlphaNum, alphaNum, parensIf)

main :: IO ()
main = loop emptyDefs

loop :: Defs -> IO ()
loop defs = do
  str <- prompt
  case runParser parseCommand () "input" str of
    Left err -> do
      print err
      loop defs
    Right (Evaluate term) -> evaluate defs term
    Right (Define name term) -> define defs name term

evaluate :: Defs -> Term -> IO ()
evaluate defs term = do
  case runCoC eval of
    Left err -> IO.putStrLn err
    Right (term', ty, norm) -> do
      IO.putStrLn ("expanded: " <> pretty term')
      IO.putStrLn ("type: " <> pretty ty)
      IO.putStrLn (pretty norm)
  loop defs
  where
    eval :: CoC (Term, Term, Term)
    eval = do
      term' <- expandDefs defs term
      ty <- reduce =<< infer emptyEnv term'
      norm <- reduce term'
      return (term', ty, norm)

define :: Defs -> Name -> Term -> IO ()
define defs name term =
  case runCoC eval of
    Left err -> do
      IO.putStrLn err
      loop defs
    Right (term', ty) -> do
      IO.putStrLn ("expanded: " <> pretty term')
      IO.putStrLn ("type: " <> pretty ty)
      loop (insertDef name term defs)
  where
    eval :: CoC (Term, Term)
    eval = do
      term' <- expandDefs defs term
      ty <- reduce =<< infer emptyEnv term'
      return (term', ty)

data Name
  = Name Text
  | NameId Text Int
  deriving (Eq, Ord)

getText :: Name -> Text
getText = \case
  Name text -> text
  NameId text i -> text <> "_" <> Text.pack (show i)

data Term
  = Type
  | Prop
  | Var Name
  | Lam Name Term Term
  | Pi Name Term Term
  | App Term Term
  deriving (Eq)

data Command
  = Define Name Term
  | Evaluate Term

parseCommand :: Parser Command
parseCommand = do
  spaces
  cmd <- try parseDefine
    <|> (Evaluate <$> parseTerm)
  eof
  return cmd

parseDefine :: Parser Command
parseDefine = do
  name <- parseName
  symbol '='
  Define name <$> parseTerm

parseTerm :: Parser Term
parseTerm = chainl1 parseFactor (return App)

parseFactor :: Parser Term
parseFactor =
  (Var <$> parseName)
  <|> (symbol 'T' >> return Type)
  <|> (symbol 'P' >> return Prop)
  <|> parseLam
  <|> parsePi
  <|> parens parseTerm

parseName :: Parser Name
parseName = do
  c <- satisfy \c -> c /= 'T' && c /= 'P' && isAlphaNum c
  str <- many alphaNum
  spaces
  return $ Name (Text.pack (c:str))

parseLam :: Parser Term
parseLam = do
  symbol '\\' <|> symbol 'λ'
  n <- parseName
  t <- squares parseTerm
  Lam n t <$> parseTerm

parsePi :: Parser Term
parsePi = do
  symbol '?' <|> symbol '∀'
  n <- parseName
  t <- squares parseTerm
  Pi n t <$> parseTerm

newtype Defs = Defs [(Name, Term)]

newtype Env = Env [(Name, Term)]

type CoC = ExceptT Text (State Int)

emptyDefs :: Defs
emptyDefs = Defs []

insertDef :: Name -> Term -> Defs -> Defs
insertDef name val (Defs d) = Defs ((name, val) : d)

expandDefs :: Defs -> Term -> CoC Term
expandDefs (Defs d) term = Monad.foldM expandDef term d
  where expandDef x (n, v) = subst n v x

emptyEnv :: Env
emptyEnv = Env []

lookupEnv :: Name -> Env -> Maybe Term
lookupEnv name (Env e) = List.lookup name e

insertEnv :: Name -> Term -> Env -> CoC Env
insertEnv name val (Env e)
  | name `elem` map fst e = throwError (getText name <> " is already defined")
  | otherwise = return $ Env ((name, val) : e)

runCoC :: CoC a -> Either Text a
runCoC act = evalState (runExceptT act) 0

fresh :: Name -> CoC Name
fresh name = do
  i <- State.get
  State.modify (1+)
  return case name of
    Name text -> NameId text i
    NameId text _ -> NameId text i

verifyKind :: Term -> CoC ()
verifyKind = \case
  Type -> return ()
  Prop -> return ()
  x -> throwError ("was expecting T or P:\n* " <> pretty x)

matchPi :: Term -> CoC (Name, Term, Term)
matchPi = \case
  Pi n x1 x2 -> return (n, x1, x2)
  x -> throwError ("was expecting a ∀ type:\n* " <> pretty x)

verifyEquiv :: Term -> Term -> CoC ()
verifyEquiv x1 x2 = do
  x1' <- reduce x1
  x2' <- reduce x2
  test <- equivalent x1' x2'
  if test then return ()
    else throwError ("could not match terms:\n* "
      <> pretty x1' <> "\n* " <> pretty x2')

equivalent :: Term -> Term -> CoC Bool
equivalent = curry \case
  (Type, Type) -> return True
  (Prop, Prop) -> return True
  (Var n1, Var n2) -> return (n1 == n2)
  (Lam n1 x1 y1, Lam n2 x2 y2) -> equivAbst (n1, x1, y1) (n2, x2, y2)
  (Pi n1 x1 y1, Pi n2 x2 y2) -> equivAbst (n1, x1, y1) (n2, x2, y2)
  (App x1 y1, App x2 y2) -> (&&) <$> equivalent x1 x2 <*> equivalent y1 y2
  (_, _) -> return False

equivAbst :: (Name, Term, Term) -> (Name, Term, Term) -> CoC Bool
equivAbst (n1, x1, y1) (n2, x2, y2) = do
  temp <- fresh (Name "#")
  let y1' = rename n1 temp y1
  let y2' = rename n2 temp y2
  (&&) <$> equivalent x1 x2 <*> equivalent y1' y2'

rename :: Name -> Name -> Term -> Term
rename n n' = \case
  Type -> Type
  Prop -> Prop
  Var n1
    | n1 == n -> Var n'
    | otherwise -> Var n1
  Lam n1 x1 x2
    | n1 == n -> Lam n1 (ren x1) x2
    | otherwise -> Lam n1 (ren x1) (ren x2)
  Pi n1 x1 x2
    | n1 == n -> Pi n1 (ren x1) x2
    | otherwise -> Pi n1 (ren x1) (ren x2)
  App x1 x2 -> App (ren x1) (ren x2)
  where ren = rename n n'

infer :: Env -> Term -> CoC Term
infer env = \case
  Type -> throwError "tried to infer type of T"
  Prop -> return Type
  Var n -> case lookupEnv n env of
    Nothing -> throwError (getText n <> " is not defined")
    Just t -> return t
  Lam n x1 x2 -> do
    t1 <- whnf =<< infer env x1
    verifyKind t1
    env' <- insertEnv n x1 env
    t2 <- infer env' x2
    return (Pi n x1 t2)
  Pi n x1 x2 -> do
    t1 <- whnf =<< infer env x1
    verifyKind t1
    env' <- insertEnv n x1 env
    t2 <- whnf =<< infer env' x2
    verifyKind t2
    return t2
  App x1 x2 -> do
    t1 <- whnf =<< infer env x1
    (n, t2, t3) <- matchPi t1
    t4 <- infer env x2
    verifyEquiv t4 t2
    subst n x2 t3

whnf :: Term -> CoC Term
whnf = \case
  App x1 x2 ->
    whnf x1 >>= \case
      Lam n _ r -> whnf =<< subst n x2 r
      x1' -> return (App x1' x2)
  x -> return x

reduce :: Term -> CoC Term
reduce = \case
  Type -> return Type
  Prop -> return Prop
  Var n -> return (Var n)
  --reduce type and body, eta-reduce if possible
  Lam n x1 x2 -> do
    x1' <- reduce x1
    x2' <- reduce x2
    return case x2' of
      App y1 y2 | y2 == Var n, not (n `freeIn` y1) -> y1
      _ -> Lam n x1' x2'
  Pi n x1 x2 -> Pi n <$> reduce x1 <*> reduce x2
  --reduce both terms, beta-reduce if possible
  App x1 x2 -> do
    x1' <- reduce x1
    x2' <- reduce x2
    case x1' of
      Lam n _ r -> reduce =<< subst n x2' r
      _ -> return (App x1' x2')

subst :: Name -> Term -> Term -> CoC Term
subst n x = \case
  Type -> return Type
  Prop -> return Prop
  Var n1
    | n1 == n -> return x
    | otherwise -> return (Var n1)
  Lam n1 x1 x2 -> substAbst Lam n x (n1, x1, x2)
  Pi n1 x1 x2 -> substAbst Pi n x (n1, x1, x2)
  App x1 x2 -> App <$> sub x1 <*> sub x2
  where sub = subst n x

substAbst :: (Name -> Term -> Term -> Term) -> Name -> Term -> (Name, Term, Term) -> CoC Term
substAbst lam n x (n1, x1, x2)
  | n1 == n = lam n1 <$> sub x1 <*> return x2
  | n1 `freeIn` x =
      if n `freeIn` x2 then do
        n1' <- fresh n1
        let x2' = rename n1 n1' x2
        lam n1' <$> sub x1 <*> sub x2'
      else lam n1 <$> sub x1 <*> return x2
  | otherwise = lam n1 <$> sub x1 <*> sub x2
  where sub = subst n x

freeIn :: Name -> Term -> Bool
freeIn n = \case
  Type -> False
  Prop -> False
  Var n1 -> n1 == n
  Lam n1 x1 x2 -> n `freeIn` x1 || (n1 /= n && n `freeIn` x2)
  Pi n1 x1 x2 -> n `freeIn` x1 || (n1 /= n && n `freeIn` x2)
  App x1 x2 -> n `freeIn` x1 || n `freeIn` x2

data Context
  = Outer
  | AppLeft
  | AppRight
  deriving (Eq)

pretty :: Term -> Text
pretty = prettyAt Outer

prettyAt :: Context -> Term -> Text
prettyAt ctx = \case
  Type -> "T"
  Prop -> "P"
  Var n -> getText n
  Lam n x1 x2 ->
    parensIf (ctx == AppLeft || ctx == AppRight) $
    "λ" <> getText n <> " [" <> pretty x1 <> "] " <> pretty x2
  Pi n x1 x2 ->
    parensIf (ctx == AppLeft || ctx == AppRight) $
    "∀" <> getText n <> " [" <> pretty x1 <> "] " <> pretty x2
  App x1 x2 ->
    parensIf (ctx == AppRight) $
    prettyAt AppLeft x1 <> " " <> prettyAt AppRight x2
