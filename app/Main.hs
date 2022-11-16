{-# LANGUAGE RecordWildCards #-}

module Main where

import Config (Config (..), getConfig)
import ConsoleBot (consoleBotLoop)
import Control.Exception (SomeException, catch)
import Control.Monad.Trans.Reader (runReaderT)
import Environment (Environment (..), LoggingLevel (..))
import Logging (printLog)
import System.IO (IOMode (..), hClose, openFile)
import TelegramBot (telegramBotLoop)
import Types.Bot (RepeatNumbersList (..), UpdateId (..))

main :: IO ()
main = do
  mbConfig <- getConfig
  case mbConfig of
    Nothing -> do
      putStrLn "Couldn't parse config"
    Just Config {..} -> do
      logHandle <- openFile "logFile.txt" AppendMode
      let env = Environment token timeout helpMessage repeatMessage repeatNumber repeatAcceptMessage repeatNumberErrorMessage loggingLevel logHandle
      runReaderT (printLog Debug "Config parsed") env
      case mode of
        "telegram" -> do
          runReaderT (printLog Release "Mode \"telegram\" was chosen") env
          runReaderT (telegramBotLoop (UpdateId 0) [] (RepeatNumbersList [])) env `catch` handleException env
        "console" -> do
          runReaderT (printLog Release "Mode \"console\" was chosen") env
          runReaderT consoleBotLoop env `catch` handleException env
        _ -> do
          runReaderT (printLog Warning "Set mode in config to \"console\" or \"telegram\"") env
          putStrLn "Set mode in config to \"console\" or \"telegram\""
      hClose logHandle

handleException :: Environment -> SomeException -> IO ()
handleException env exception = do
  runReaderT (printLog Error $ "Exception thrown: " ++ show exception ++ "\n Program terminated") env
  putStr $ "Exception thrown: " ++ show exception ++ "\n Program terminated"
