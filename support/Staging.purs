module Config (config) where

type Config =
    { clearNexusURL :: String
    }

config :: Config
config = { clearNexusURL : "https://staging.clearnex.us/" }
