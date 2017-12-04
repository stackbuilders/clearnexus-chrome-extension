{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GenerateClient.Types ( CreateMailingData(..)
                            , MailingData(..)
                            , ClickEventData(..)
                            , CreateLinkData(..)
                            , LastMailingData  ) where


import Data.Text (Text)
import GHC.Generics

data CreateLinkData = CreateLinkData
  { target_email :: !Text
  , description :: !Text
  } deriving (Generic)

data ClickEventData = ClickEventData
  { subscribed :: !Bool
  , time       :: !Text
  , ip         :: !(Maybe Text)
  , user_agent  :: !(Maybe Text)
  } deriving (Generic)

data CreateMailingData = CreateMailingData {
    link_email :: !Text
  , description :: !Text
  } deriving Generic

data MailingData = MailingData
  { email :: !Text
  , organization :: !Text
  , token :: !Text
  , is_link_subscribed :: !Bool
  , unsubscription_link :: !Text
  , subscription_link  :: !Text
  , created_at    :: !Text
  , click_events  :: ![ClickEventData]
  } deriving Generic

newtype LastMailingData = LastMailingData {
  mailing_data :: Maybe MailingData
  } deriving Generic
