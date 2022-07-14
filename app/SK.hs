{-# LANGUAGE BlockArguments, OverloadedStrings, LambdaCase #-}

module SK where

import Text.Parsec (runParser, (<|>), try, chainl1, satisfy, spaces, eof)

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO

import Util (prompt, Parser, symbol, parens, isAlphaNum, parensIf)

main :: IO ()
main = loop []

loop :: [(Name, Term)] -> IO ()
loop defs = do
  txt <- prompt
  case runParser parseCommand () "input" txt of
    Left err -> do
      print err
      loop defs
    Right (Evaluate x) -> do
      IO.putStrLn (pretty (evaluate x))
      loop defs
    Right (Define n x) ->
      loop ((n, x) : defs)
  where
    evaluate x = reduce (foldl' subst x defs)
    pretty = prettyP False

newtype Name = Name { getName :: Char }
  deriving (Eq)

data Term
  = S
  | K
  | Var Name
  | App Term Term

data Command
  = Evaluate Term
  | Define Name Term

parseName :: Parser Name
parseName = do
  c <- satisfy \c -> isAlphaNum c && c /= 's' && c /= 'k'
  spaces
  return (Name c)

parseCommand :: Parser Command
parseCommand = do
  spaces
  cmd <- try parseDefine <|> (Evaluate <$> parseTerm)
  eof
  return cmd

parseDefine :: Parser Command
parseDefine = do
  n <- parseName
  symbol '='
  Define n <$> parseTerm

parseTerm :: Parser Term
parseTerm = chainl1 parseFactor (return App)

parseFactor :: Parser Term
parseFactor =
  (symbol 's' >> return S) <|>
  (symbol 'k' >> return K) <|>
  (Var <$> parseName) <|>
  parens parseTerm

subst :: Term -> (Name, Term) -> Term
subst y (n, x) = case y of
  Var n' | n' == n -> x
  App xL xR -> App (subst xL (n, x)) (subst xR (n, x))
  _ -> y

reduce :: Term -> Term
reduce = \case
  App x y -> apply (reduce x, reduce y)
  x -> x

apply :: (Term, Term) -> Term
apply = \case
  (App K x, _) -> x
  (App (App S x) y, z) -> apply (apply (x, z), apply (y, z))
  (x, y) -> App x y

prettyP :: Bool -> Term -> Text
prettyP appRight = \case
  S -> "s"
  K -> "k"
  Var n -> Text.singleton (getName n)
  App x y -> parensIf appRight (prettyP False x <> prettyP True y)
