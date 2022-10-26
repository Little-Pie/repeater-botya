{-# LANGUAGE RecordWildCards #-}

module Main where

import Config (Config (..), getConfig)
import ConsolBot (consolBotLoop)
import Control.Exception (SomeException, catch)
import Control.Monad.Trans.Reader (runReaderT)
import Environment (Environment (..))
import Logging (printDebug, printError, printRelease, printWarning)
import System.IO (IOMode (..), hClose, openFile)
import TelegramBot (telegramBotLoop)

main :: IO ()
main = do
  mbConfig <- getConfig
  case mbConfig of
    Nothing -> do
      putStrLn "Couldn't parse config"
    Just Config {..} -> do
      logHandle <- openFile "logFile.txt" AppendMode
      let env = Environment token timeout helpMessage repeatMessage repeatNumber repeatAcceptMessage repeatNumberErrorMessage loggingLevel logHandle
      runReaderT (printDebug "Config parsed") env
      case mode of
        "telegram" -> do
          runReaderT (printRelease "Mode \"telegram\" was chosen") env
          runReaderT (telegramBotLoop 0 [] []) env `catch` handleException env
        "consol" -> do
          runReaderT (printRelease "Mode \"consol\" was chosen") env
          runReaderT consolBotLoop env `catch` handleException env
        _ -> do
          runReaderT (printWarning "Set mode in config to \"consol\" or \"telegram\"") env
          putStrLn "Set mode in config to \"consol\" or \"telegram\""
      hClose logHandle

handleException :: Environment -> SomeException -> IO ()
handleException env exception = do
  runReaderT (printError $ "Exception thrown: " ++ show exception ++ "\n Program terminated") env
  putStr $ "Exception thrown: " ++ show exception ++ "\n Program terminated"
