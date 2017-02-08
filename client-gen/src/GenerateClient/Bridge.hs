module GenerateClient.Bridge where

import Data.Proxy
import GenerateClient.Types
import GenerateClient.API
import Servant.PureScript ( defaultBridge )
import Language.PureScript.Bridge ( buildBridge
                                  , mkSumType )

data MyBridge

myBridge = buildBridge defaultBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

myApiProxy :: Proxy GetEmailPropertiesR
myApiProxy = Proxy

myTypes = [ mkSumType ( Proxy :: Proxy EmailProperties )
          , mkSumType ( Proxy :: Proxy Token )
          , mkSumType ( Proxy :: Proxy UriEmail )
          ]
