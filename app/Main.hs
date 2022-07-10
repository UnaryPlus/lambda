{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main (main) where

import System.Environment (getArgs)

import qualified Lambda
import qualified CoC

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["lambda"] -> Lambda.main
    ["coc"] -> CoC.main
    _ -> putStrLn "expecting argument: 'lambda' or 'coc'"
