{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module SK where

import Text.Parsec (runParser, (<|>), try, many1, chainl1, spaces, eof)
import Data.List (foldl')
import Data.Text (Text, pack)
import qualified Data.Text.IO as IO

import Util (prompt, Parser, symbol, parens, lowerNum, parensIf)

main :: IO ()
main = loop []

loop :: [(Name, Term)] -> IO ()
loop defs = do
  txt <- prompt
  case runParser parseCommand () "input" txt of
    Left err -> print err >> loop defs
    Right (Evaluate x) -> do
      let x' = foldl' subst x defs
      case reduce x' of
        Left n -> notDefined n
        Right x'' -> do
          IO.putStrLn ("- " <> pretty x')
          IO.putStrLn ("- " <> pretty x'')
      loop defs
    Right (Define n x) ->
       case reduce (foldl' subst x defs) of
         Left m -> notDefined m >> loop defs
         Right _ -> loop ((n, x) : defs)
  where
    notDefined n = IO.putStrLn (getName n <> " is not defined")
    pretty = prettyP False

newtype Name = Name { getName :: Text }
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
  str <- many1 lowerNum
  spaces
  return (Name (pack str))

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
  (symbol 'S' >> return S) <|>
  (symbol 'K' >> return K) <|>
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
  App (App (App S x) y) z -> do
    x' <- reduce x
    y' <- reduce y
    z' <- reduce z
    Right (App (App x' z') (App y' z'))
  App (App K x) _ -> reduce x
  App x y -> App <$> reduce x <*> reduce y

prettyP :: Bool -> Term -> Text
prettyP appLeft = \case
  S -> "S"
  K -> "K"
  Var n -> getName n
  App x y -> parensIf appLeft (prettyP True x <> " " <> prettyP False y)
