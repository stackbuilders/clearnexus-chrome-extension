{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Lib
import Data.Aeson
import Data.Text ( Text )
import Data.Time ( UTCTime )
import Net.Types ( IPv4 )
import Servant

type PostLinksR =
  QueryParam "access_token" Token :>
  "api" :>
  "links" :>
  ReqBody '[JSON] CreateLinkData :>
  Post '[JSON] LinkData

newtype Token = Token {unToken :: Text}
  deriving ( Eq
           , Show
           , ToJSON
           , FromJSON )

data CreateLinkData =
  CreateLinkData {cldTargetEmail :: !Text}

instance FromJSON CreateLinkData where
  parseJSON = withObject "CreateLinkData" $ \o ->
    CreateLinkData <$> ( o .: "target_email" )

data LinkData
  = LinkData { ldEmail :: !Text
             , ldOrganization :: !Text
             , ldToken :: !Token
             , ldUnsubsciptionLink :: !Text
             , ldSubscriptionLink :: !Text
             , ldCreatedAt :: !UTCTime
             , ldClickEvents :: ![ClickEventData] }

instance ToJSON LinkData where
  toJSON LinkData {..} = object
    [ "email"        .= ldEmail
    , "organization" .= ldOrganization
    , "token"        .= ldToken
    , "unsubscription_link" .= ldUnsubsciptionLink
    , "subscription_link"   .= ldSubscriptionLink
    , "created_at"   .= ldCreatedAt
    , "click_events" .= ldClickEvents ]


data ClickEventData
  = ClickEventData { cedSubscribed :: !Bool
                   , cedTime :: !UTCTime
                   , cedIp :: !( Maybe IPv4 )
                   , cedUserAgent :: !( Maybe Text ) }

instance ToJSON ClickEventData where
  toJSON ClickEventData {..} = object
    [ "subscribed" .= cedSubscribed
    , "time"       .= cedTime
    , "ip"         .= cedIp
    , "user_agent" .= cedUserAgent ]

main :: IO ()
main = someFunc
