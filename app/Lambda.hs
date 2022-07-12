{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}

module Lambda (main, Term(..), Name(..), runReduce, pretty) where

import Data.List (foldl')

import Text.Parsec (runParser, (<|>), try, many1, chainl1, spaces, eof)

import Control.Monad.State (State, evalState)
import qualified Control.Monad.State as State

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO

import Util (prompt, Parser, symbol, parens, alphaNum, parensIf)

main :: IO ()
main = loop []

loop :: [(Name, Term)] -> IO ()
loop env = do
  str <- prompt
  case runParser parseCommand () "input" str of
    Left err -> do
      print err
      loop env
    Right (Evaluate term) -> do
      let res = runReduce (withEnv env term)
      IO.putStrLn (pretty res)
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

withEnv :: [(Name, Term)] -> Term -> Term
withEnv env term = foldl' addDef term env
  where addDef t (n, v) = App (Lam n t) v

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
  <|> parseLam
  <|> parens parseTerm

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

runReduce :: Term -> Term
runReduce t = evalState (reduce t) 0

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
  --reduce body, eta-reduce if possible
  Lam n r -> do
    r' <- reduce r
    case r' of
      App t1 t2 | t2 == Var n, not (n `freeIn` t1) -> return t1
      _ -> return (Lam n r')
  --reduce both terms, beta-reduce if possible
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
    | n1 `freeIn` t ->
        if n `freeIn` r then do
          n1' <- fresh n1
          r' <- subst n1 (Var n1') r
          Lam n1' <$> subst n t r'
        else return (Lam n1 r)
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

pretty :: Term -> Text
pretty = prettyAt Outer

prettyAt :: Context -> Term -> Text
prettyAt ctx = \case
  Var n -> prettyName n
  Lam n r ->
    parensIf (ctx == AppLeft || ctx == AppRight) $
    "λ" <> prettyName n <> ". " <> prettyAt LamRight r
  App t1 t2 ->
    parensIf (ctx == AppRight) $
    prettyAt AppLeft t1 <> " " <> prettyAt AppRight t2

prettyName :: Name -> Text
prettyName = \case
  Name text -> text
  NameId text x -> text <> "_" <> Text.pack (show x)
