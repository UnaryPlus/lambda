{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}

module CoC (main) where

import Data.List (foldl')
import Data.Functor.Identity (runIdentity)
import System.IO (hFlush, stdout)

import Data.Map (Map)
import qualified Data.Map as Map

import Text.Parsec (Parsec, runParser, (<|>), char, try, many, lower, digit, alphaNum, spaces, eof)

import Control.Monad.Trans (lift)
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Control.Monad.State (StateT, State, mapStateT, evalStateT)
import qualified Control.Monad.State as State

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO

main :: IO ()
main = evalStateT (loop []) 0

loop :: [(Name, Term, Term)] -> StateT Int IO ()
loop env = do
  lift (IO.putStr "> " >> hFlush stdout)
  str <- lift IO.getLine
  case runParser parseCommand () "input" str of
    Left err -> do
      lift (print err)
      loop env
    Right (Evaluate term) -> evaluate env term
    Right (Define name term) -> define env name term

evaluate :: [(Name, Term, Term)] -> Term -> StateT Int IO ()
evaluate env term = do
  res <- convertCoC (inferReduce (withEnv env term))
  case res of
    Left err -> lift (IO.putStrLn err)
    Right (ty, term') -> do
      lift $ IO.putStrLn ("type: " <> pretty Outer ty)
      lift $ IO.putStrLn (pretty Outer term')
  loop env

define :: [(Name, Term, Term)] -> Name -> Term -> StateT Int IO ()
define env name term = do
  res <- convertCoC (infer Map.empty (withEnv env term))
  case res of
    Left err -> do
      lift (IO.putStrLn err)
      loop env
    Right ty -> do
      lift $ IO.putStrLn ("type: " <> pretty Outer ty)
      loop ((name, ty, term) : env)

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

type Parser = Parsec Text ()

withEnv :: [(Name, Term, Term)] -> Term -> Term
withEnv env term = foldl' addDef term env
  where addDef x (n, t, v) = App (Lam n t x) v

symbol :: Char -> Parser ()
symbol c = char c >> spaces

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
parseTerm = parseApp =<< parseFactor

parseApp :: Term -> Parser Term
parseApp fun = do
    arg <- parseFactor
    parseApp (App fun arg)
  <|> return fun

parseFactor :: Parser Term
parseFactor =
  (Var <$> parseName)
  <|> (symbol 'T' >> return Type)
  <|> (symbol 'P' >> return Prop)
  <|> parseLam
  <|> parsePi
  <|> parseParens

parseName :: Parser Name
parseName = do
  c <- lower <|> digit
  str <- many alphaNum
  spaces
  return $ Name (Text.pack (c:str))

parseLam :: Parser Term
parseLam = do
  symbol '\\' <|> symbol 'λ'
  n <- parseName
  symbol '['
  t <- parseTerm
  symbol ']'
  Lam n t <$> parseTerm

parsePi :: Parser Term
parsePi = do
  symbol '?' <|> symbol '∀'
  n <- parseName
  symbol '['
  t <- parseTerm
  symbol ']'
  Pi n t <$> parseTerm

parseParens :: Parser Term
parseParens = do
  symbol '('
  t <- parseTerm
  symbol ')'
  return t

type Env = Map Name Term

type CoC = ExceptT Text (State Int)

convertCoC :: CoC a -> StateT Int IO (Either Text a)
convertCoC = mapStateT (return . runIdentity) . runExceptT

inferReduce :: Term -> CoC (Term, Term)
inferReduce term = (,) <$> infer Map.empty term <*> reduce term

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
  x -> throwError ("was expecting T or P:\n* " <> pretty Outer x)

matchPi :: Term -> CoC (Name, Term, Term)
matchPi = \case
  Pi n x1 x2 -> return (n, x1, x2)
  x -> throwError ("was expecting a pi type:\n* " <> pretty Outer x)

verifyEquiv :: Term -> Term -> CoC ()
verifyEquiv x1 x2 = do
  test <- equivalent x1 x2
  if test then return ()
    else throwError ("could not match terms:\n* "
      <> pretty Outer x1 <> "\n* "
      <> pretty Outer x2)

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
  Var n -> case Map.lookup n env of
    Nothing -> throwError (getText n <> " is not defined")
    Just t -> return t
  Lam n x1 x2 -> do
    _ <- infer env x1
    t2 <- infer (Map.insert n x1 env) x2
    let ty = Pi n x1 t2
    _ <- infer env ty
    return ty
  Pi n x1 x2 -> do
    t1 <- reduce =<< infer env x1
    verifyKind t1
    t2 <- reduce =<< infer (Map.insert n x1 env) x2
    verifyKind t2
    return t2
  App x1 x2 -> do
    t1 <- reduce =<< infer env x1
    (n, t2, t3) <- matchPi t1
    t4 <- reduce =<< infer env x2
    verifyEquiv t4 t2
    subst n x2 t3

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
  | n1 `freeIn` x = do
      n1' <- fresh n1
      let x2' = rename n1 n1' x2
      lam n1' <$> sub x1 <*> sub x2'
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

pretty :: Context -> Term -> Text
pretty ctx = \case
  Type -> "T"
  Prop -> "P"
  Var n -> getText n
  Lam n x1 x2 ->
    parensIf (ctx == AppLeft || ctx == AppRight) $
    "λ" <> getText n <> " [" <> pretty Outer x1 <> "] " <> pretty Outer x2
  Pi n x1 x2 ->
    parensIf (ctx == AppLeft || ctx == AppRight) $
    "∀" <> getText n <> " [" <> pretty Outer x1 <> "] " <> pretty Outer x2
  App x1 x2 ->
    parensIf (ctx == AppRight) $
    pretty AppLeft x1 <> " " <> pretty AppRight x2

parensIf :: Bool -> Text -> Text
parensIf cond text
  | cond = "(" <> text <> ")"
  | otherwise = text
