module Config ( setEnv
              , Config(..)
              , loadConfig
              , CHROME   ) where


import Prelude (Unit, bind, pure)
import Control.Monad.Aff (Aff)
import DOM (DOM)
import Data.Maybe (Maybe)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Foreign.Null (Null(..))
import Data.Function.Uncurried (Fn2, runFn2)


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


type EnvSetter = { storage ::
                      { sync ::
                           { set :: Fn2 Unit Unit Unit }
                      }
                 }


foreign import loadEnvironment :: forall eff r . Aff (dom :: DOM | eff) { environment :: String | r }

foreign import uncurriedSaveEnv :: forall eff . Fn2 (Null EnvSetter) String (Eff (console :: CONSOLE | eff) Unit)


loadConfig :: forall eff . Aff (dom :: DOM | eff) Config
loadConfig = do
  items <- loadEnvironment
  case items.environment of
    "development" -> pure development
    "staging" -> pure staging
    _ -> pure production

setEnv :: forall eff .
          Maybe EnvSetter
       -> String
       ->  Eff (console :: CONSOLE | eff) Unit
setEnv chrome env = (runFn2 uncurriedSaveEnv) (Null chrome) env
