{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GenerateClient.Types ( EmailProperties
                            , LinkData
                            , ClickEventData   ) where


import Data.Text (Text)
import GHC.Generics

data EmailProperties = EmailProperties
  { subscribed :: !Bool
  , link_token:: !Text
  }
    deriving (Generic)

data ClickEventData = ClickEventData
  { subscribed :: !Bool
  , time       :: !Text
  , ip         :: !(Maybe Int)
  , user_agent  :: !(Maybe Text)
  } deriving (Generic)

data LinkData = LinkData
  { email        :: !Text
  , organization :: !Text
  , token        :: !Text
  , unsubscription_link :: !Text
  , subscription_link  :: !Text
  , created_at    :: !Text
  , click_events  :: ![ClickEventData]
  } deriving (Generic)
