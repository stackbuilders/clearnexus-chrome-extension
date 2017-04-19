module Util ( getSubscriptionStatus
            , postNewLink
            , getLink
            , EPInstances(..)     ) where


import Control.Monad.Aff (Aff)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Either (Either)
import Data.Eq (class Eq, eq)
import Data.Show (class Show, show)
import GenerateClient.Types (EmailProperties(..), CreateLinkData(..), LinkData)
import Network.HTTP.Affjax (AJAX)
import Prelude (($), (<<<))
import Prim (String)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import ServerAPI (SPParams_(..), getApiEmailByEmail, getApiLinkByToken, postApiLinks)


type AjaxRequest eff a = ExceptT AjaxError
                                (ReaderT (SPSettings_ SPParams_)
                                (Aff (ajax :: AJAX | eff))) a


newtype EPInstances = EPInstances EmailProperties


instance showEPInstances :: Show EPInstances where
  show (EPInstances (EmailProperties { subscribed })) =
    show subscribed


instance eqEPInstances :: Eq EPInstances where
  eq (EPInstances (EmailProperties { subscribed: s1 }))
     (EPInstances (EmailProperties { subscribed: s2 })) = eq s1 s2


makeSettings :: { baseURL :: String } -> SPSettings_ SPParams_
makeSettings uri = defaultSettings $ SPParams_ uri

getSubscriptionStatus :: forall eff .
		         String
                      -> String
                      -> String
                      -> Aff (ajax :: AJAX | eff) (Either AjaxError EmailProperties)
getSubscriptionStatus url email token =
  (runReaderT <<< runExceptT)
    (getApiEmailByEmail email token)
       (makeSettings { baseURL: url })

getLink :: forall eff .
           String
        -> String
        -> String
        -> Aff (ajax :: AJAX | eff) (Either AjaxError LinkData)
getLink url linkToken accessToken =
  (runReaderT <<< runExceptT)
    (getApiLinkByToken linkToken accessToken)
       (makeSettings { baseURL: url })

postNewLink :: forall eff .
               String
            -> String
            -> String
            -> Aff (ajax :: AJAX | eff) (Either AjaxError LinkData)
postNewLink url email token =
  let createLinkData = CreateLinkData { target_email: email }
  in (runReaderT <<< runExceptT)
       (postApiLinks createLinkData token)
         (makeSettings { baseURL: url })
