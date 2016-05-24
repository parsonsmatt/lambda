module Main where

import Control.Monad.State

import Lambda.Untyped

main :: IO ()
main = do
    mapM_ putStrLn
        [ "~~~"
        , "λ Lambda Repl λ"
        , "~~~"
        ]
    evalStateT repl mempty
