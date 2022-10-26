{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), Value (..), decodeStrict, (.:))
import qualified Data.ByteString as B (ByteString, readFile)
import Environment (LoggingLevel)

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

instance FromJSON Config where
  parseJSON (Object config) =
    Config
      <$> config .: "token"
      <*> config .: "timeout"
      <*> config .: "helpMessage"
      <*> config .: "repeatMessage"
      <*> config .: "repeatNumber"
      <*> config .: "repeatAcceptMessage"
      <*> config .: "repeatNumberErrorMessage"
      <*> config .: "mode"
      <*> config .: "loggingLevel"
  parseJSON _ = mzero

getConfig :: IO (Maybe Config)
getConfig = do
  rawJSON <- B.readFile "config.json"
  let result = decodeStrict rawJSON :: Maybe Config
  case result of
    Nothing -> return Nothing
    Just conf -> return $ Just conf
