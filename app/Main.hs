{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main (main) where

import System.Environment (getArgs)

import qualified Lambda
import qualified CoC
import qualified SystemF
import qualified SK
import qualified HM

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> Lambda.main
    ["lambda"] -> Lambda.main
    ["coc"] -> CoC.main
    ["systemf"] -> SystemF.main
    ["sk"] -> SK.main
    ["hm"] -> HM.main
    _ -> putStrLn "expecting argument: 'lambda', 'coc', 'systemf', 'sk', 'hm'"
