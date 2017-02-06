{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Lib
import Data.Text ( Text )
import Net.Types ( IPv4 )

type PostLinksR =
  QueryParam "access_token" Token :>
  "api" :>
  "links" :>
  ReqBody '[JSON] CreateLinkData :>
  Post '[JSON] LinkData

newtype Token = Token {unToken :: Text}

newtype CreateLinkData =
  CreateLinkData {cldTargetEmail :: !Text}

data LinkData
  = LinkData { ldEmail :: !Text
             , ldOrganization :: !Text
             , ldToken :: !Token
             , ldUnsubsciptionLink :: !Text
             , ldSubscriptionLink :: !Text
             , ldCreatedAt :: !UTCTime
             , ldClickEvents :: ![ClickEventData] }

data ClickEventData
  = ClickEventData { cedSubscribed :: !Bool
                   , cedTime :: !UTCTime
                   , cedIp :: !(Maybe IPv4)
                   , cedUserAgent :: !Maybe Text}

main :: IO ()
main = someFunc
