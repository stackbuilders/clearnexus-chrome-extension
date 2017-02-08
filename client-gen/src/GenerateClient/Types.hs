module GenerateClient.Types
  ( EmailProperties
  , Token
  , UriEmail
  ) where

import Data.Text ( Text )

data EmailProperties = EmailProperties
  { subscribed :: !Bool }

newtype Token = Token { unToken :: Text }

newtype UriEmail = UriEmail
  { unUriEmail :: Text }
