{-
  (c) 2022 Owen Bechtel
  License: MIT (see LICENSE)
-}

{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main (main) where

import System.Environment (getArgs)

import qualified Lambda
import qualified SK
import qualified SystemF
import qualified HM
import qualified CoC

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> Lambda.main
    ["lambda"] -> Lambda.main
    ["sk"] -> SK.main
    ["systemf"] -> SystemF.main
    ["hm"] -> HM.main
    ["coc"] -> CoC.main
    _ -> putStrLn "expecting argument: 'lambda', 'sk', 'systemf', 'hm', or 'coc'"
