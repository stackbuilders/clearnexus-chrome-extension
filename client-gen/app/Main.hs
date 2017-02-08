module Main where

import System.Environment ( lookupEnv )

main :: IO ()
main = do
  maybePath <- lookupEnv "PS_CLIENT_DIRECTORY"
  maybe ( putStrLn "Please set the PS_CLIENT_DIRECTORY environment variable" ) putStrLn maybePath
