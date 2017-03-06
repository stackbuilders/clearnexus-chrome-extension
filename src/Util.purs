module Util where


import Control.Monad.Aff (Aff)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Either (Either)
import Data.Eq (class Eq , eq)
import Function (($))
import Data.Show (class Show , show)
import GenerateClient.Types (EmailProperties(..))
import Prelude ((<<<))
import Prim (String)
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import ServerAPI (SPParams_(..) , getApiEmailByEmail)


type AjaxRequest eff a = ExceptT AjaxError
                                (ReaderT (SPSettings_ SPParams_)
                                (Aff (ajax :: AJAX | eff))) a


newtype EPInstances = EPInstances EmailProperties


instance showEPInstances :: Show EPInstances where
  show (EPInstances (EmailProperties { subscribed })) =
    show subscribed

instance eqEPInstances :: Eq EPInstances where
  eq (EPInstances (EmailProperties { subscribed : s1 }))
     (EPInstances (EmailProperties { subscribed : s2 } )) = eq s1 s2


getSubscriptionStatus' :: forall eff .
                          String
                       -> String
                       -> AjaxRequest eff EmailProperties
getSubscriptionStatus' = getApiEmailByEmail


getSubscriptionStatus :: forall eff .
			 { baseURL :: String }
                      -> String
                      -> String
                      -> Aff (ajax :: AJAX | eff)
                             (Either AjaxError EmailProperties)
getSubscriptionStatus url email token =
  (runReaderT <<< runExceptT)
    (getSubscriptionStatus' email token)
      (makeSettings url)


makeSettings :: { baseURL :: String } -> SPSettings_ SPParams_
makeSettings uri = defaultSettings $ SPParams_ uri
