{-# LANGUAGE RecordWildCards #-}

module Logging where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Environment (App, Environment (..), LoggingLevel (..))
import System.IO (hFlush, hPutStrLn)

printLog :: LoggingLevel -> String -> App ()
printLog = printLogHelper

printLogHelper :: LoggingLevel -> String -> App ()
printLogHelper logLvl str = do
  Environment {..} <- ask
  if loggingLevel <= logLvl
    then do
      liftIO $ hPutStrLn logHandle str
      liftIO $ hFlush logHandle
    else pure ()
