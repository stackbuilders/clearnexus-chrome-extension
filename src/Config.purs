module Config ( Config(..)
              , environment
              , loadConfig
              , CHROME   ) where


-- << CHANGE THIS VARIABLE ACCORDING TO ENVIRONMENT >> --
environment :: String
environment = "development"

foreign import data CHROME :: !

data Config = Config {
  url :: String
  }

development :: Config
development = Config {
  url: "https://staging.clearnex.us/"
  }

staging :: Config
staging = Config {
  url: "https://staging.clearnex.us/"
  }

production :: Config
production = Config {
  url: "https://clearnex.us/"
  }

loadConfig :: Config
loadConfig = do
  case environment of
    "development" -> development
    "staging" -> staging
    _ -> production
