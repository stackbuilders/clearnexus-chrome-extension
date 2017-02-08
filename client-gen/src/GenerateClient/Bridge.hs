module GenerateClient.Bridge where

import Data.Proxy
import GenerateClient.Types
import GenerateClient.API

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

myApiProxy :: Proxy GetEmailPropertiesR
myApiProxy = Proxy

myTypes :: [HaskellType]
myTypes = [ mkTypeInfo ( Proxy :: Proxy EmailProperties )
          , mkTypeInfo ( Proxy :: Proxy Token )
          , mkTypeInfo ( Proxy :: Proxy UriEmail )
          ]
