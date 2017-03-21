module Main where


import GenerateClient.Bridge
import Language.PureScript.Bridge ( writePSTypes )
import Servant.PureScript (writeAPIModule)
import System.Environment (lookupEnv)


main :: IO ()
main = do
  let path = "../src"
  writeAPIModule path myBridgeProxy myApiProxy
  writePSTypes path myBridge myTypes
