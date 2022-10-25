{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), Value (..), decodeStrict, (.:))
import qualified Data.ByteString as B (ByteString, readFile)

data Config = Config
  { token :: String,
    timeout :: Int,
    helpMessage :: String,
    repeatMessage :: String,
    repeatNumber :: Int,
    repeatAcceptMessage :: String,
    errorMessage :: String,
    mode :: String
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
      <*> config .: "errorMessage"
      <*> config .: "mode"
  parseJSON _ = mzero

getConfig :: IO (Maybe Config)
getConfig = do
  rawJSON <- B.readFile "config.json"
  let result = decodeStrict rawJSON :: Maybe Config
  case result of
    Nothing -> return Nothing
    Just conf -> return $ Just conf
