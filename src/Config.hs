{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Aeson (FromJSON, decodeStrict)
import Data.ByteString as B (readFile)
import Environment (LoggingLevel)
import GHC.Generics (Generic)

data Config = Config
  { token :: String,
    timeout :: Int,
    helpMessage :: String,
    repeatMessage :: String,
    repeatNumber :: Int,
    repeatAcceptMessage :: String,
    repeatNumberErrorMessage :: String,
    mode :: String,
    loggingLevel :: LoggingLevel
  }
  deriving (Generic, FromJSON)

getConfig :: IO (Maybe Config)
getConfig = do
  rawJSON <- B.readFile "config.json"
  pure $ decodeStrict rawJSON
