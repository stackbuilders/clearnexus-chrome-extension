-- File auto generated by servant-purescript! --
module ServerAPI where


import Prelude
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader.Class (ask, class MonadReader)
import Data.Argonaut.Generic.Aeson (decodeJson)
import GenerateClient.Types (EmailProperties)
import Network.HTTP.Affjax (AJAX)
import Prim (String)
import Servant.PureScript.Affjax (AjaxError, affjax, defaultRequest)
import Servant.PureScript.Settings (SPSettings_(SPSettings_))
import Servant.PureScript.Util (encodeQueryItem, encodeURLPiece, getResult)

newtype SPParams_ = SPParams_ { baseURL :: String }


getApiEmailByEmail :: forall eff m . ( MonadReader ( SPSettings_ SPParams_ ) m
                                     , MonadError AjaxError m
                                     , MonadAff ( ajax :: AJAX | eff) m ) =>
                                       String -> String -> m EmailProperties
getApiEmailByEmail email access_token = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let baseURL = spParams_.baseURL
  let httpMethod = "GET"
  let reqUrl = baseURL <> "api" <> "/" <> "email"
        <> "/" <> encodeURLPiece spOpts_' email
        <> "?" <> encodeQueryItem spOpts_' "access_token" access_token
  let reqHeaders =
        []
  let affReq = defaultRequest
                 { method = httpMethod
                 , url = reqUrl
                 , headers = defaultRequest.headers <> reqHeaders
                 }
  affResp <- affjax affReq
  getResult affReq decodeJson affResp
