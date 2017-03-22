module Config where

import Prelude (bind, pure)
import Control.Monad.Aff (Aff(..))
import DOM (DOM)

data Config = Config {
  url :: String
}

devConfig = Config {
  url: "https://localhost"
}
stagingConfig = Config {
  url: "https://staging.clearnex.us"
}
prodConfig = Config {
  url: "https://clearnex.us"
}

foreign import loadEnvironment :: forall e. Aff (dom :: DOM | e) String

loadConfig :: forall e. Aff (dom :: DOM | e) Config
loadConfig = do
  environment <- loadEnvironment
  case environment of
    "dev" -> pure devConfig
    "stage" -> pure stagingConfig
    _ -> pure prodConfig
