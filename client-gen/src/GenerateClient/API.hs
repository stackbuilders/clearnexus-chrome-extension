module GenerateClient.API
  ( GetEmailPropertiesR )
where

type GetEmailPropertiesR =
  QueryParam "access_token" Token :>
  "api" :>
  "email" :>
  Capture "email" UriEmail :>
  Get '[JSON] EmailProperties
