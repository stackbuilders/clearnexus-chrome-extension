{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module GenerateClient.API
  ( GetEmailPropertiesR )
where

import Data.Text ( Text )
import GenerateClient.Types
import Servant

type GetEmailPropertiesR =
  QueryParam "access_token" Text :>
  "api" :>
  "email" :>
  Capture "email" Text :>
  Get '[JSON] EmailProperties
