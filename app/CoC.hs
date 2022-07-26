{-
  (c) 2022 Owen Bechtel
  License: MIT (see LICENSE)
-}

{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}

module CoC (main) where

import qualified Data.List as List
import qualified Control.Monad as Monad
import Control.Monad ((>=>))

import Text.Parsec (runParser, (<|>), char, digit, many1, notFollowedBy, try, chainl1, chainr1, spaces, eof)

import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Control.Monad.State (State, evalState)
import qualified Control.Monad.State as State

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Data.Set (Set)
import qualified Data.Set as Set

import Util (prompt, Parser, symbol, parens, alphaNum, alpha, parensIf)

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
    Right (ty, norm) -> do
      IO.putStrLn (": " <> pretty ty)
      IO.putStrLn (pretty norm)
  loop defs
  where
    eval :: CoC (Term, Term)
    eval = do
      term' <- expandDefs defs term
      ty <- reduce =<< infer emptyEnv term'
      norm <- reduce term'
      return (ty, norm)

define :: Defs -> Name -> Term -> IO ()
define defs name term =
  case runCoC eval of
    Left err -> do
      IO.putStrLn err
      loop defs
    Right ty -> do
      IO.putStrLn (": " <> pretty ty)
      loop (insertDef name term defs)
  where
    eval :: CoC Term
    eval = do
      term' <- expandDefs defs term
      reduce =<< infer emptyEnv term'

data Name
  = Name Text
  | NameId Text Int
  deriving (Eq, Ord)

data Bind
  = Bind Name
  | Throwaway
  deriving (Eq)

showName :: Name -> Text
showName = \case
  Name text -> text
  NameId text i -> text <> "_" <> Text.pack (show i)

showBind :: Bind -> Text
showBind = \case
  Bind n -> showName n
  Throwaway -> "_"

data Term
  = Type Int --should be non-negative
  | Var Name
  | Lam Bind Term Term
  | Pi Bind Term Term
  | App Term Term

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
parseTerm = chainr1 app arrow
  where
    app = chainl1 parseFactor (return App)
    arrow = do
      (char '-' >> symbol '>') <|> symbol '→'
      return (Pi Throwaway)

parseFactor :: Parser Term
parseFactor =
  (Var <$> parseName)
  <|> parseSort
  <|> parseLam
  <|> parsePi
  <|> parens parseTerm

parseName :: Parser Name
parseName = do
  notFollowedBy (char 'T' >> notFollowedBy alpha)
  str <- many1 alphaNum
  spaces
  return $ Name (Text.pack str)

parseBind :: Parser Bind
parseBind =
  (symbol '_' >> return Throwaway)
  <|> (Bind <$> parseName)

parseSort :: Parser Term
parseSort = do
  _ <- char 'T'
  i <- (read <$> many1 digit) <|> return 0
  spaces
  return (Type i)

parseLam :: Parser Term
parseLam = do
  symbol '\\' <|> symbol 'λ'
  n <- parseBind
  symbol ':'
  t <- parseTerm
  symbol '.'
  Lam n t <$> parseTerm

parsePi :: Parser Term
parsePi = do
  symbol '?' <|> symbol '∀'
  n <- parseBind
  symbol ':'
  t <- parseTerm
  symbol '.'
  Pi n t <$> parseTerm

type CoC = ExceptT Text (State Int)

runCoC :: CoC a -> Either Text a
runCoC act = evalState (runExceptT act) 0

newtype Defs = Defs [(Name, Term)]

emptyDefs :: Defs
emptyDefs = Defs []

insertDef :: Name -> Term -> Defs -> Defs
insertDef name val (Defs d) = Defs ((name, val) : d)

expandDefs :: Defs -> Term -> CoC Term
expandDefs (Defs d) term = Monad.foldM expandDef term d
  where expandDef x (n, v) = subst n v x

newtype Env = Env [(Name, Term)]

emptyEnv :: Env
emptyEnv = Env []

lookupEnv :: Name -> Env -> Maybe Term
lookupEnv name (Env e) = List.lookup name e

insertEnv :: Bind -> Term -> Env -> CoC Env
insertEnv bind val (Env e) = case bind of
  Bind name
    | name `elem` map fst e -> throwError (showName name <> " is already defined")
    | otherwise -> return $ Env ((name, val) : e)
  Throwaway -> return (Env e)

fresh :: Name -> CoC Name
fresh name = do
  i <- State.get
  State.modify (1+)
  return case name of
    Name text -> NameId text i
    NameId text _ -> NameId text i

level :: Term -> CoC Int
level = whnf >=> \case
  Type i -> return i
  x -> throwError ("was expecting a sort:\n* " <> pretty x)

matchPi :: Term -> CoC (Bind, Term, Term)
matchPi = whnf >=> \case
  Pi n x1 x2 -> return (n, x1, x2)
  x -> throwError ("was expecting a ∀ type:\n* " <> pretty x)

runIncludes :: Term -> Term -> CoC ()
runIncludes x1 x2 = do
  x1' <- reduce x1
  x2' <- reduce x2
  i <- State.get
  includes x1' x2'
  State.put i

includes :: Term -> Term -> CoC ()
includes = curry \case
  (Type i, Type j)
    | i <= j -> return ()
    | otherwise -> throwError (pretty (Type j)
        <> " does not contain " <> pretty (Type i))
  (Pi n1 x1 y1, Pi n2 x2 y2) -> checkAbst (n1, x1, y1) (n2, x2, y2) includes
  (x1, x2) -> equivalent x1 x2

equivalent :: Term -> Term -> CoC ()
equivalent = curry \case
  (Type i, Type j) | i == j -> return ()
  (Var n1, Var n2) | n1 == n2 -> return ()
  (Lam n1 x1 y1, Lam n2 x2 y2) -> checkAbst (n1, x1, y1) (n2, x2, y2) equivalent
  (Pi n1 x1 y1, Pi n2 x2 y2) -> checkAbst (n1, x1, y1) (n2, x2, y2) equivalent
  (App x1 y1, App x2 y2) -> equivalent x1 x2 >> equivalent y1 y2
  (x1, x2) -> throwError ("could not match terms:\n* "
    <> pretty x1 <> "\n* " <> pretty x2)

checkAbst :: (Bind, Term, Term) -> (Bind, Term, Term) -> (Term -> Term -> CoC ()) -> CoC ()
checkAbst (b1, x1, y1) (b2, x2, y2) check = do
  equivalent x1 x2
  case (b1, b2) of
    (Bind n1, Bind n2) -> do
      temp <- fresh n1
      let y1' = rename n1 temp y1
      let y2' = rename n2 temp y2
      check y1' y2'
    _ -> check y1 y2

rename :: Name -> Name -> Term -> Term
rename n n' = \case
  Type i -> Type i
  Var n1
    | n1 == n -> Var n'
    | otherwise -> Var n1
  Lam n1 x1 x2
    | n1 == Bind n -> Lam n1 (ren x1) x2
    | otherwise -> Lam n1 (ren x1) (ren x2)
  Pi n1 x1 x2
    | n1 == Bind n -> Pi n1 (ren x1) x2
    | otherwise -> Pi n1 (ren x1) (ren x2)
  App x1 x2 -> App (ren x1) (ren x2)
  where ren = rename n n'

infer :: Env -> Term -> CoC Term
infer env = \case
  Type i -> return (Type (i + 1))
  Var n -> case lookupEnv n env of
    Nothing -> throwError (showName n <> " is not defined")
    Just t -> return t
  Lam n x1 x2 -> do
    _ <- level =<< infer env x1
    env' <- insertEnv n x1 env
    t2 <- infer env' x2
    _ <- infer env (Pi n x1 t2)
    return (Pi n x1 t2)
  Pi n x1 x2 -> do
    i1 <- level =<< infer env x1
    env' <- insertEnv n x1 env
    i2 <- level =<< infer env' x2
    let i = if i2 == 0 then 0 else max i1 i2
    return (Type i)
  App x1 x2 -> do
    (n, tA, tB) <- matchPi =<< infer env x1
    t2 <- infer env x2
    runIncludes t2 tA
    subst n x2 tB

whnf :: Term -> CoC Term
whnf = \case
  App x1 x2 ->
    whnf x1 >>= \case
      Lam n _ r -> whnf =<< subst n x2 r
      x1' -> return (App x1' x2)
  x -> return x

reduce :: Term -> CoC Term
reduce = \case
  Type i -> return (Type i)
  Var n -> return (Var n)
  --reduce type and body, eta-reduce if possible
  Lam b x1 x2 -> do
    x1' <- reduce x1
    x2' <- reduce x2
    return case x2' of
      App y1 (Var n') | Bind n <- b, n' == n, not (n `freeIn` y1) -> y1
      _ -> Lam b x1' x2'
  Pi n x1 x2 -> Pi n <$> reduce x1 <*> reduce x2
  --reduce both terms, beta-reduce if possible
  App x1 x2 -> do
    x1' <- reduce x1
    x2' <- reduce x2
    case x1' of
      Lam n _ r -> reduce =<< subst n x2' r
      _ -> return (App x1' x2')

class SubstName a where
  subst :: a -> Term -> Term -> CoC Term

instance SubstName Name where
  subst n x = \case
    Type i -> return (Type i)
    Var n1
      | n1 == n -> return x
      | otherwise -> return (Var n1)
    Lam n1 x1 x2 -> substAbst n x Lam (n1, x1, x2)
    Pi n1 x1 x2 -> substAbst n x Pi (n1, x1, x2)
    App x1 x2 -> App <$> sub x1 <*> sub x2
    where sub = subst n x

instance SubstName Bind where
  subst b x = case b of
    Bind n -> subst n x
    Throwaway -> return

substAbst :: Name -> Term -> (Bind -> Term -> Term -> Term) -> (Bind, Term, Term) -> CoC Term
substAbst n x lam (b, x1, x2)
  | b /= Bind n && n `freeIn` x2 = case b of
      Bind n1 -> do
        n1' <- fresh n1
        let x2' = rename n1 n1' x2
        lam (Bind n1') <$> sub x1 <*> sub x2'
      Throwaway -> lam Throwaway <$> sub x1 <*> sub x2
  | otherwise = lam b <$> sub x1 <*> return x2
  where sub = subst n x

freeIn :: Name -> Term -> Bool
freeIn n = \case
  Type _ -> False
  Var n1 -> n1 == n
  Lam n1 x1 x2 -> n `freeIn` x1 || (n1 /= Bind n && n `freeIn` x2)
  Pi n1 x1 x2 -> n `freeIn` x1 || (n1 /= Bind n && n `freeIn` x2)
  App x1 x2 -> n `freeIn` x1 || n `freeIn` x2

data Context
  = Outer
  | ArrowLeft
  | ArrowRight
  | AppLeft
  | AppRight
  deriving (Eq)

pretty :: Term -> Text
pretty = prettyAt Outer Set.empty

prettyAt :: Context -> Set Name -> Term -> Text
prettyAt ctx env = \case
  Type 0 -> "T"
  Type i -> "T" <> Text.pack (show i)
  Var n -> showName n
  Pi Throwaway x1 x2 ->
    parensIf (ctx == ArrowLeft || ctx == AppLeft || ctx == AppRight) $
    prettyAt ArrowLeft env x1 <> " → " <> prettyAt ArrowRight env x2
  Lam n x1 x2 -> prettyAbst ctx env "λ" (n, x1, x2)
  Pi n x1 x2 -> prettyAbst ctx env "∀" (n, x1, x2)
  App x1 x2 ->
    parensIf (ctx == AppRight) $
    prettyAt AppLeft env x1 <> " " <> prettyAt AppRight env x2

prettyAbst :: Context -> Set Name -> Text -> (Bind, Term, Term) -> Text
prettyAbst ctx env sym (b, x1, x2) =
  parensIf (ctx == ArrowLeft || ctx == AppLeft || ctx == AppRight) $
  sym <> showBind b'
  <> ":" <> parensIf (long x1) (prettyAt Outer env x1)
  <> ". " <> prettyAt Outer env' x2'
  where
    (b', x2') = case b of
      Bind n@(NameId text _)
        | Name text `Set.member` env || Name text `freeIn` x2 -> (b, x2)
        | otherwise -> (Bind (Name text), rename n (Name text) x2)
      _ -> (b, x2)
    env' = case b' of
      Bind n -> Set.insert n env
      Throwaway -> env

long :: Term -> Bool
long = \case
  Type _ -> False
  Var _ -> False
  _ -> True
