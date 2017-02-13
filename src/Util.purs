module Util where

import Control.Monad.Aff ( Aff )
import Control.Monad.Except.Trans ( ExceptT )
import Control.Monad.Reader.Trans ( ReaderT )
import Data.Eq ( class Eq
               , eq
               )
import Data.Show ( class Show
                 , show
                 )
import GenerateClient.Types ( EmailProperties(..)
                            , UriEmail
                            , Token
                            )
import Network.HTTP.Affjax ( AJAX )
import Servant.PureScript.Affjax ( AjaxError )
import Servant.PureScript.Settings ( SPSettings_ )
import ServerAPI ( SPParams_
                 , getApiEmailByEmail
                 )

getSubscriptionStatus :: forall eff.
                         UriEmail
                      -> Token
                      -> ExceptT AjaxError
			   ( ReaderT ( SPSettings_ SPParams_ )
			     ( Aff ( ajax :: AJAX | eff ) ) )
                               EmailProperties
getSubscriptionStatus = getApiEmailByEmail

newtype EPInstances = EPInstances EmailProperties

instance showEPInstances :: Show EPInstances where
  show ( EPInstances ( EmailProperties { subscribed } ) ) =
    show subscribed

instance eqEPInstances :: Eq EPInstances where
  eq ( EPInstances ( EmailProperties { subscribed : s1 } ) )
       ( EPInstances ( EmailProperties { subscribed : s2 } ) ) =
         eq s1 s2
