{-
  (c) 2022 Owen Bechtel
  License: MIT (see LICENSE)
-}

{-# LANGUAGE BlockArguments, OverloadedStrings #-}

module Util
  ( prompt
  , Parser, symbol, parens, squares
  , isAlphaNum, alphaNum, lowerNum, alpha
  , parensIf
  ) where

import System.IO (hFlush, stdout)
import Data.Char (isAsciiUpper, isAsciiLower, isDigit)

import Data.Text (Text)
import qualified Data.Text.IO as IO

import Text.Parsec (Parsec, between, char, satisfy, spaces, between)

prompt :: IO Text
prompt = IO.putStr "> " >> hFlush stdout >> IO.getLine

type Parser = Parsec Text ()

symbol :: Char -> Parser ()
symbol c = char c >> spaces

parens :: Parser a -> Parser a
parens = between (symbol '(') (symbol ')')

squares :: Parser a -> Parser a
squares = between (symbol '[') (symbol ']')

isAlphaNum :: Char -> Bool
isAlphaNum c = isAsciiUpper c || isAsciiLower c || isDigit c

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

lowerNum :: Parser Char
lowerNum = satisfy \c -> isAsciiLower c || isDigit c

alpha :: Parser Char
alpha = satisfy \c -> isAsciiUpper c || isAsciiLower c

parensIf :: Bool -> Text -> Text
parensIf cond text
  | cond = "(" <> text <> ")"
  | otherwise = text
