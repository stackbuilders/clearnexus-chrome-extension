{-# LANGUAGE OverloadedStrings #-}

module GenerateClient.Bridge ( myApiProxy
                             , myBridge
                             , myBridgeProxy
                             , myTypes     ) where


import Control.Lens.Getter (view)
import Data.Proxy (Proxy(..))
import GenerateClient.Types
import GenerateClient.API
import Servant.PureScript (defaultBridge, HasBridge(..))
import Language.PureScript.Bridge ( buildBridge
                                  , BridgePart
                                  , FullBridge
                                  , mkSumType
                                  , TypeInfo(..)
                                  , (^==)
                                  , (<|>)
                                  , psTypeParameters
                                  , typeModule
                                  , haskType       )


data MyBridge

instance HasBridge MyBridge where
  languageBridge _ = buildBridge myBridgePart


fixTypesModule :: BridgePart
fixTypesModule = do
  typeModule ^== "ClearNexus.Client"
  t <- view haskType
  TypeInfo (_typePackage t) "ClearNexus.Client.Types" (_typeName t)
    <$> psTypeParameters

myBridge :: FullBridge
myBridge = buildBridge myBridgePart

myBridgePart :: BridgePart
myBridgePart = defaultBridge <|> fixTypesModule

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

myApiProxy :: Proxy API
myApiProxy = Proxy

myTypes = [ mkSumType (Proxy :: Proxy EmailProperties)
          , mkSumType (Proxy :: Proxy Token)
          , mkSumType (Proxy :: Proxy UriEmail)
          , mkSumType (Proxy :: Proxy ClickEventData)
          , mkSumType (Proxy :: Proxy LinkData)
          ]
