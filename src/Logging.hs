{-# LANGUAGE RecordWildCards #-}

module Logging where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Environment (App, Environment (..), LoggingLevel (..))
import System.IO (Handle, hFlush, hPutStrLn)

printDebug :: String -> App ()
printDebug str = do
  Environment {..} <- ask
  if loggingLevel < Release
    then liftIO $ printLog logHandle str
    else pure ()

printRelease :: String -> App ()
printRelease str = do
  Environment {..} <- ask
  if loggingLevel < Warning
    then liftIO $ printLog logHandle str
    else pure ()

printWarning :: String -> App ()
printWarning str = do
  Environment {..} <- ask
  if loggingLevel < Error
    then liftIO $ printLog logHandle str
    else pure ()

printError :: String -> App ()
printError str = do
  Environment {..} <- ask
  liftIO $ printLog logHandle str

printLog :: Handle -> String -> IO ()
printLog logHandle str = do
  hPutStrLn logHandle str
  hFlush logHandle
