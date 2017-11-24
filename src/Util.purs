module Util ( getLastMailing
            , postNewMailing
            , postNewLink
            , AjaxRequest
            ) where


import Control.Monad.Aff (Aff)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Either (Either)
import Data.Eq (class Eq, eq)
import Data.Show (class Show, show)
import GenerateClient.Types 
import Network.HTTP.Affjax (AJAX)
import Prelude (($), (<<<))
import Prim (String)
import Servant.PureScript.Affjax (AjaxError)
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import ServerAPI (SPParams_(..), getApiMailingLastByEmail, postApiMailings, postApiLinks)


type AjaxRequest eff a =
  ExceptT AjaxError (ReaderT (SPSettings_ SPParams_) (Aff (ajax :: AJAX | eff))) a


makeSettings :: { baseURL :: String } -> SPSettings_ SPParams_
makeSettings uri = defaultSettings $ SPParams_ uri

getLastMailing :: forall eff . 
                  String 
               -> String
               -> String
               -> Aff (ajax :: AJAX | eff) (Either AjaxError LastMailingData)
getLastMailing url email access_token = 
  (runReaderT <<< runExceptT) (getApiMailingLastByEmail email access_token) (makeSettings { baseURL: url })

postNewMailing :: forall eff .
                  String
               -> String
               -> String
               -> String
               -> Aff (ajax :: AJAX | eff) (Either AjaxError MailingData)
postNewMailing url email desc access_token = 
  let createMailingData = CreateMailingData { link_email: email, description: desc }
  in (runReaderT <<< runExceptT) (postApiMailings createMailingData access_token) (makeSettings { baseURL: url })

postNewLink :: forall eff .
               String
            -> String
            -> String
            -> String
            -> Aff (ajax :: AJAX | eff) (Either AjaxError MailingData)
postNewLink url email desc access_token = 
  let createLinkData = CreateLinkData { target_email: email, description: desc }
  in (runReaderT <<< runExceptT) (postApiLinks createLinkData access_token) (makeSettings { baseURL: url })






