{-# LANGUAGE OverloadedStrings #-}

module Environment where

import Control.Monad (mzero)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson (FromJSON (..), Value (String))
import System.IO (Handle)

data LoggingLevel = Debug | Release | Warning | Error
  deriving (Eq, Ord)

instance FromJSON LoggingLevel where
  parseJSON (String logLevel) = case logLevel of
    "Debug" -> pure Debug
    "Release" -> pure Release
    "Warning" -> pure Warning
    "Error" -> pure Error
    _ -> mzero
  parseJSON _ = mzero

data Environment = Environment
  { token :: String,
    timeout :: Int,
    helpMessage :: String,
    repeatMessage :: String,
    repeatNumber :: Int,
    repeatAcceptMessage :: String,
    repeatNumberErrorMessage :: String,
    loggingLevel :: LoggingLevel,
    logHandle :: Handle
  }

type App a = ReaderT Environment IO a
