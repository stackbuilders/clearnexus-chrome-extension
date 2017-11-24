{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module GenerateClient.API (API) where


import Data.Text (Text)
import GenerateClient.Types
import Servant

type API = GetMailingR
       :<|> GetLastMailingR
       :<|> PostLinksR
       :<|> PostMailingsR

type Token = Text

type GetMailingR =
  QueryParam "access_token" Token :>
  "api" :>
  "mailing" :>
  Capture "token" Token :>
  Get '[JSON] MailingData

type GetLastMailingR =
  QueryParam "access_token" Token :>
  "api" :>
  "mailing" :>
  "last" :>
  Capture "email" Text :> 
  Get '[JSON] LastMailingData

type PostLinksR =
  QueryParam "access_token" Token :>
  "api" :>
  "links" :>
  ReqBody '[JSON] CreateLinkData :>
  PostCreated '[JSON] MailingData

type PostMailingsR =
  QueryParam "access_token" Token :>
  "api" :>
  "mailings" :>
  ReqBody '[JSON] CreateMailingData :>
  PostCreated '[JSON] MailingData
