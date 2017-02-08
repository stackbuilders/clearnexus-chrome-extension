module Main where

import GenerateClient.Bridge
import System.Environment ( lookupEnv )

main :: IO ()
main = do
  maybePath <- lookupEnv "PS_CLIENT_DIRECTORY"
  case maybePath of
    Nothing ->
      putStrLn
        "Please set the PS_CLIENT_DIRECTORY environment variable"
    Just path -> do
      writeAPIModule path myBridgeProxy myApiProxy
      writePSTypes path defaultBridge myTypes
