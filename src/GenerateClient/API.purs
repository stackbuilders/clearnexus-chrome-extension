module GenerateClient.API where


import Data.Generic (class Generic)


newtype CreateLinkData = CreateLinkData { target_email :: String }

derive instance genericCreateLinkData :: Generic CreateLinkData
