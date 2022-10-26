{-# LANGUAGE RecordWildCards #-}

module Main where

import Config (Config (..), getConfig)
import ConsolBot (consolBotLoop)
import Control.Exception (SomeException, catch)
import Control.Monad.Trans.Reader (runReaderT)
import Environment (Environment (..))
import Logging ()
import System.IO (IOMode (..), hClose, openFile)
import TelegramBot (telegramBotLoop)

main :: IO ()
main = do
  mbConfig <- getConfig
  case mbConfig of
    Nothing -> putStrLn "Couldn't parse config"
    Just Config {..} -> do
      logHandle <- openFile "logFile.txt" AppendMode
      let env = Environment token timeout helpMessage repeatMessage repeatNumber repeatAcceptMessage repeatNumberErrorMessage loggingLevel logHandle
      case mode of
        "telegram" -> runReaderT (telegramBotLoop 0 [] []) env `catch` handleException
        "consol" -> runReaderT consolBotLoop env `catch` handleException
        _ -> putStrLn "Set mode in config to \"consol\" or \"telegram\""
      hClose logHandle

handleException :: SomeException -> IO ()
handleException exception = putStr $ "Exception thrown: " ++ show exception ++ "\n Program terminated"
