module GenerateClient.Bridge where

import Data.Proxy
import GenerateClient.Types
import GenerateClient.API
import Servant.PureScript ( defaultBridge )
import Language.PureScript.Bridge ( buildBridge )
import Language.PureScript.Bridge.TypeInfo

data MyBridge

myBridge = buildBridge defaultBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

myApiProxy :: Proxy GetEmailPropertiesR
myApiProxy = Proxy

myTypes :: [HaskellType]
myTypes = [ mkTypeInfo ( Proxy :: Proxy EmailProperties )
          , mkTypeInfo ( Proxy :: Proxy Token )
          , mkTypeInfo ( Proxy :: Proxy UriEmail )
          ]
