{-# LANGUAGE DeriveGeneric #-}

module GenerateClient.Types ( EmailProperties
                            , Token
                            , UriEmail
                            , LinkData
                            , ClickEventData   ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Net.Types (IPv4)


data EmailProperties = EmailProperties
  { subscribed :: !Bool }
    deriving (Generic)

newtype Token = Token { unToken :: Text }
    deriving (Generic)

newtype UriEmail = UriEmail
  { unUriEmail :: Text }
    deriving (Generic)

data ClickEventData = ClickEventData
  { cedSubscribed :: !Bool
  , cedTime       :: !UTCTime
  , cedIp         :: !(Maybe IPv4)
  , cedUserAgent  :: !(Maybe Text)
  } deriving (Generic)

data LinkData = LinkData
  { ldEmail        :: !Text
  , ldOrganization :: !Text
  , ldToken        :: !Token
  , ldUnsubsciptionLink :: !Text
  , ldSubscriptionLink  :: !Text
  , ldCreatedAt    :: !UTCTime
  , ldClickEvents  :: ![ClickEventData]
  } deriving (Generic)
