{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Environment where

import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import System.IO (Handle)

data LoggingLevel = Debug | Release | Warning | Error
  deriving (Eq, Ord, Generic, FromJSON)

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
