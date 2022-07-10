{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}

module CoC where

import Control.Monad (forever)

import Data.Map (Map)
import qualified Data.Map as Map

import Text.Parsec (Parsec, runParser, (<|>), char, many, lower, digit, alphaNum, spaces, eof)

import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Control.Monad.State (State, evalState)
import qualified Control.Monad.State as State

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO

main :: IO ()
main = forever do
  str <- IO.getLine
  case runParser parseInput () "input" str of
    Left err -> print err
    Right term ->
      case runCoC (reduceInput term) of
        Left err -> IO.putStrLn err
        Right (ty, term') -> do
          IO.putStrLn ("type: " <> pretty Outer ty)
          IO.putStrLn (pretty Outer term')

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
  | Prod Name Term Term
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
  <|> (symbol 'T' >> return Type)
  <|> (symbol 'P' >> return Prop)
  <|> parseLam
  <|> parseProd
  <|> parseParens

parseName :: Parser Name
parseName = do
  c <- lower <|> digit
  str <- many alphaNum
  spaces
  return $ Name (Text.pack (c:str))

parseLam :: Parser Term
parseLam = do
  symbol '\\'
  n <- parseName
  symbol '['
  t <- parseTerm
  symbol ']'
  Lam n t <$> parseTerm

parseProd :: Parser Term
parseProd = do
  symbol '?'
  n <- parseName
  symbol '['
  t <- parseTerm
  symbol ']'
  Prod n t <$> parseTerm

parseParens :: Parser Term
parseParens = do
  symbol '('
  t <- parseTerm
  symbol ')'
  return t

symbol :: Char -> Parser ()
symbol c = char c >> spaces

type Env = Map Name Term

type CoC = ExceptT Text (State Int)

runCoC :: CoC a -> Either Text a
runCoC act = evalState (runExceptT act) 0

reduceInput :: Term -> CoC (Term, Term)
reduceInput term = (,) <$> infer Map.empty term <*> reduce term

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
  _ -> throwError "was expecting Type or Prop"

matchProd :: Term -> CoC (Name, Term, Term)
matchProd = \case
  Prod n x1 x2 -> return (n, x1, x2)
  _ -> throwError "was expecting a product type"

verifyEquiv :: Term -> Term -> CoC ()
verifyEquiv x1 x2 = do
  test <- equivalent x1 x2
  if test then return ()
    else throwError "could not match terms"

equivalent :: Term -> Term -> CoC Bool
equivalent = curry \case
  (Type, Type) -> return True
  (Prop, Prop) -> return True
  (Var n1, Var n2) -> return (n1 == n2)
  (Lam n1 x1 y1, Lam n2 x2 y2) -> equivAbst (n1, x1, y1) (n2, x2, y2)
  (Prod n1 x1 y1, Prod n2 x2 y2) -> equivAbst (n1, x1, y1) (n2, x2, y2)
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
  Prod n1 x1 x2
    | n1 == n -> Prod n1 (ren x1) x2
    | otherwise -> Prod n1 (ren x1) (ren x2)
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
    t1 <- infer env x1
    verifyKind t1
    t2 <- infer (Map.insert n x1 env) x2
    return (Prod n x1 t2)
  Prod n x1 x2 -> do
    t1 <- infer env x1
    verifyKind t1
    t2 <- infer (Map.insert n x1 env) x2
    verifyKind t2
    return t2
  App x1 x2 -> do
    t1 <- reduce =<< infer env x1
    (n, t2, t3) <- matchProd t1
    t4 <- reduce =<< infer env x2
    verifyEquiv t4 t2
    subst n x2 t3

reduce :: Term -> CoC Term
reduce = \case
  Type -> return Type
  Prop -> return Prop
  Var n -> return (Var n)
  Lam n x1 x2 -> Lam n <$> reduce x1 <*> reduce x2
  Prod n x1 x2 -> Prod n <$> reduce x1 <*> reduce x2
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
  Prod n1 x1 x2 -> substAbst Prod n x (n1, x1, x2)
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
  Prod n1 x1 x2 -> n `freeIn` x1 || (n1 /= n && n `freeIn` x2)
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
    "\\" <> getText n <> " [" <> pretty Outer x1 <> "] " <> pretty Outer x2
  Prod n x1 x2 ->
    parensIf (ctx == AppLeft || ctx == AppRight) $
    "?" <> getText n <> " [" <> pretty Outer x1 <> "] " <> pretty Outer x2
  App x1 x2 ->
    parensIf (ctx == AppRight) $
    pretty AppLeft x1 <> " " <> pretty AppRight x2

parensIf :: Bool -> Text -> Text
parensIf cond text
  | cond = "(" <> text <> ")"
  | otherwise = text
