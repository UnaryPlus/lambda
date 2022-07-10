{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main (main) where

import Text.Parsec (Parsec, runParser, (<|>), char, many1, alphaNum, spaces, eof)

import Control.Monad.State (State, evalState)
import qualified Control.Monad.State as State

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.Text.IO as IO

main :: IO ()
main = do
  str <- IO.getLine
  case runParser parseTerm () "input" str of
    Left err -> print err
    Right term ->
      let res = evalState (reduce term) 0
      in IO.putStrLn (pretty Outer res)
  main

data Name
  = Name Text
  | NameId Text Int
  deriving (Eq)

data Term
  = Var Name
  | Lam Name Term
  | App Term Term

type Parser = Parsec Text ()

parseInput :: Parser Term
parseInput = do
  spaces
  term <- parseTerm
  eof
  return term

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
  _ <- char '\\'
  spaces
  name <- parseName
  _ <- char '.'
  spaces
  Lam name <$> parseTerm

parseParens :: Parser Term
parseParens = do
  _ <- char '('
  spaces
  term <- parseTerm
  _ <- char ')'
  spaces
  return term

type Reduce = State Int

fresh :: Name -> Reduce Name
fresh name = do
  new <- State.get
  State.modify (1+)
  case name of
    Name text -> return (NameId text new)
    NameId text _ -> return (NameId text new)

reduce :: Term -> Reduce Term
reduce = \case
  App (Lam n r) t -> do
    t' <- reduce t
    reduce =<< subst n t' r
  Var n -> return (Var n)
  Lam n r -> Lam n <$> reduce r
  App t1 t2 -> App <$> reduce t1 <*> reduce t2

subst :: Name -> Term -> Term -> Reduce Term
subst n t = \case
  Var n1
    | n1 == n -> return t
    | otherwise -> return (Var n1)
  Lam n1 r
    | n1 == n -> return (Lam n1 r)
    | n1 `freeIn` t -> do
        n1' <- fresh n1
        t' <- subst n1 (Var n1') t
        Lam n1 <$> subst n t' r
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
    "\\" <> prettyName n <> ". " <> pretty LamRight r
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
