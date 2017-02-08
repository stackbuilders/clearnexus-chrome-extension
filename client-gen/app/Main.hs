module Main where

import System.Environment ( lookupEnv )

main :: IO ()
main = do
  maybePath <- lookupEnv "PS_CLIENT_DIRECTORY"
  maybe
    ( putStrLn "Please set the PS_CLIENT_DIRECTORY environment variable" )
    ( \path' -> let path = path' ) maybePath
  writeAPIModule path myBridgeProxy myApiProxy
  writePSTypes path defaultBridge myTypes
