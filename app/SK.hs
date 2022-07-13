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
    Left err -> print err >> loop defs
    Right (Evaluate x) -> do
      case evaluate x of
        Left n -> notDefined n
        Right x' -> IO.putStrLn (pretty x')
      loop defs
    Right (Define n x) ->
       case evaluate x of
         Left m -> notDefined m >> loop defs
         Right _ -> loop ((n, x) : defs)
  where
    evaluate x = reduce (foldl' subst x defs)
    notDefined n = putStrLn (getName n : " is not defined")
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

reduce :: Term -> Either Name Term
reduce = \case
  S -> Right S
  K -> Right K
  Var n -> Left n
  App x y ->
    reduce x >>= \case
      App (App S a) b -> reduce (App (App a y) (App b y))
      App K a -> Right a
      x' -> App x' <$> reduce y

prettyP :: Bool -> Term -> Text
prettyP appRight = \case
  S -> "s"
  K -> "k"
  Var n -> Text.singleton (getName n)
  App x y -> parensIf appRight (prettyP False x <> prettyP True y)
