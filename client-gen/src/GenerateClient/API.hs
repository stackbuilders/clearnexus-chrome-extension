{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module GenerateClient.API
  ( GetEmailPropertiesR )
where

import GenerateClient.Types
import Servant

type GetEmailPropertiesR =
  QueryParam "access_token" Token :>
  "api" :>
  "email" :>
  Capture "email" UriEmail :>
  Get '[JSON] EmailProperties
