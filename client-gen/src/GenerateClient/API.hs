{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module GenerateClient.API (API) where


import Data.Text (Text)
import GenerateClient.Types
import Servant
import GenerateClient.Types (CreateLinkData)


type GetLinkR =
  QueryParam "access_token" Text :>
  "api" :>
  "link" :>
  Capture "token" Text :>
  Get '[JSON] LinkData

type GetEmailPropertiesR =
  QueryParam "access_token" Text :>
  "api" :>
  "email" :>
  Capture "email" Text :>
  Get '[JSON] EmailProperties

type PostLinksR =
  QueryParam "access_token" Text :>
  "api" :>
  "links" :>
  ReqBody '[JSON] CreateLinkData :>
  Post '[JSON] LinkData

type API = GetEmailPropertiesR :<|> PostLinksR :<|> GetLinkR
