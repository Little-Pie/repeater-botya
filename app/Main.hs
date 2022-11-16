{-# LANGUAGE RecordWildCards #-}

module Main where

import Config (Config (..), getConfig)
import ConsoleBot (consoleBotLoop)
import Control.Exception (SomeException, catch)
import Control.Monad.Trans.Reader (runReaderT)
import Environment (Environment (..))
import Logging (printDebug, printError, printRelease, printWarning)
import System.IO (IOMode (..), hClose, openFile)
import TelegramBot (telegramBotLoop)

main :: IO ()
main = do
  eithConfig <- getConfig
  case eithConfig of
    Left errorMsg -> do
      putStrLn errorMsg
    Right Config {..} -> do
      logHandle <- openFile "logFile.txt" AppendMode
      let env = Environment token timeout helpMessage repeatMessage repeatNumber repeatAcceptMessage repeatNumberErrorMessage loggingLevel logHandle
      runReaderT (printDebug "Config parsed") env
      case mode of
        "telegram" -> do
          runReaderT (printRelease "Mode \"telegram\" was chosen") env
          runReaderT (telegramBotLoop 0 [] []) env `catch` handleException env
        "console" -> do
          runReaderT (printRelease "Mode \"console\" was chosen") env
          runReaderT consoleBotLoop env `catch` handleException env
        _ -> do
          runReaderT (printWarning "Set mode in config to \"console\" or \"telegram\"") env
          putStrLn "Set mode in config to \"console\" or \"telegram\""
      hClose logHandle

handleException :: Environment -> SomeException -> IO ()
handleException env exception = do
  runReaderT (printError $ "Exception thrown: " ++ show exception ++ "\n Program terminated") env
  putStr $ "Exception thrown: " ++ show exception ++ "\n Program terminated"
