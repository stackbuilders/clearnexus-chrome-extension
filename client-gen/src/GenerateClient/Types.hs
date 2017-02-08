{-# LANGUAGE DeriveGeneric #-}

module GenerateClient.Types
  ( EmailProperties
  , Token
  , UriEmail
  ) where

import Data.Text ( Text )
import GHC.Generics ( Generic )

data EmailProperties = EmailProperties
  { subscribed :: !Bool }
    deriving ( Generic )

newtype Token = Token { unToken :: Text }
    deriving ( Generic )

newtype UriEmail = UriEmail
  { unUriEmail :: Text }
    deriving ( Generic )
