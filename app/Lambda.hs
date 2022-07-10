{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}

module Lambda where

import Data.List (foldl')
import Data.Functor.Identity (runIdentity)
import System.IO (hFlush, stdout)

import Text.Parsec (Parsec, runParser, (<|>), char, try, many1, alphaNum, spaces, eof)

import Control.Monad.Trans (lift)
import Control.Monad.State (State, StateT, evalStateT, mapStateT)
import qualified Control.Monad.State as State

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO

main :: IO ()
main = evalStateT (loop []) 0

loop :: [(Name, Term)] -> StateT Int IO ()
loop env = do
  lift (IO.putStr "> " >> hFlush stdout)
  str <- lift IO.getLine
  case runParser parseCommand () "input" str of
    Left err -> do
      lift (print err)
      loop env
    Right (Evaluate term) -> do
      res <- mapStateT (return . runIdentity) (reduce (withEnv env term))
      lift (IO.putStrLn (pretty Outer res))
      loop env
    Right (Define name term) -> do
      loop ((name, term) : env)

data Name
  = Name Text
  | NameId Text Int
  deriving (Eq)

data Term
  = Var Name
  | Lam Name Term
  | App Term Term
  deriving (Eq)

data Command
  = Define Name Term
  | Evaluate Term

type Parser = Parsec Text ()

withEnv :: [(Name, Term)] -> Term -> Term
withEnv env term = foldl' addDef term env
  where addDef t (n, v) = App (Lam n t) v

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
  <|> parseLam
  <|> parseParens

parseName :: Parser Name
parseName = do
  str <- many1 alphaNum
  spaces
  return $ Name (Text.pack str)

parseLam :: Parser Term
parseLam = do
  symbol '\\' <|> symbol 'λ'
  name <- parseName
  symbol '.'
  Lam name <$> parseTerm

parseParens :: Parser Term
parseParens = do
  symbol '('
  term <- parseTerm
  symbol ')'
  return term

fresh :: Name -> State Int Name
fresh name = do
  new <- State.get
  State.modify (1+)
  case name of
    Name text -> return (NameId text new)
    NameId text _ -> return (NameId text new)

reduce :: Term -> State Int Term
reduce = \case
  Var n -> return (Var n)
  -- reduce body, eta-reduce if possible
  Lam n r -> do
    r' <- reduce r
    case r' of
      App t1 t2 | t2 == Var n, not (n `freeIn` t1) -> return t1
      _ -> return (Lam n r')
  -- reduce both terms, beta-reduce if possible
  App t1 t2 -> do
    t1' <- reduce t1
    t2' <- reduce t2
    case t1' of
      Lam n r -> reduce =<< subst n t2' r
      _ -> return (App t1' t2')

subst :: Name -> Term -> Term -> State Int Term
subst n t = \case
  Var n1
    | n1 == n -> return t
    | otherwise -> return (Var n1)
  Lam n1 r
    | n1 == n -> return (Lam n1 r)
    | n1 `freeIn` t -> do
        n1' <- fresh n1
        r' <- subst n1 (Var n1') r
        Lam n1' <$> subst n t r'
    | otherwise -> Lam n1 <$> subst n t r
  App t1 t2 -> App <$> subst n t t1 <*> subst n t t2

freeIn :: Name -> Term -> Bool
freeIn n = \case
  Var n1 -> n1 == n
  Lam n1 r -> n1 /= n && n `freeIn` r
  App t1 t2 -> n `freeIn` t1 || n `freeIn` t2

data Context
  = Outer
  | AppLeft
  | AppRight
  | LamRight
  deriving (Eq)

pretty :: Context -> Term -> Text
pretty ctx = \case
  Var n -> prettyName n
  Lam n r ->
    parensIf (ctx == AppLeft || ctx == AppRight) $
    "λ" <> prettyName n <> ". " <> pretty LamRight r
  App t1 t2 ->
    parensIf (ctx == AppRight) $
    pretty AppLeft t1 <> " " <> pretty AppRight t2

prettyName :: Name -> Text
prettyName = \case
  Name text -> text
  NameId text x -> text <> "_" <> Text.pack (show x)

parensIf :: Bool -> Text -> Text
parensIf cond text
  | cond = "(" <> text <> ")"
  | otherwise = text
