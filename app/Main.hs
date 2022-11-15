{-# LANGUAGE RecordWildCards #-}

module Main where

import Config (Config (..), getConfig)
import ConsoleBot (consoleBotLoop)
import Control.Exception (Handler (..), SomeException, catch, catches)
import Control.Monad.Trans.Reader (runReaderT)
import Environment (Environment (..))
import Logging (printDebug, printError, printRelease, printWarning)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
import System.IO (IOMode (..), hClose, openFile)
import TelegramBot (runTelegramBot)

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
          runReaderT runTelegramBot env `catches` [Handler $ handleHttpException env, Handler $ handleException env]
        "console" -> do
          runReaderT (printRelease "Mode \"console\" was chosen") env
          runReaderT consoleBotLoop env `catch` handleException env
        _ -> do
          runReaderT (printWarning "Set mode in config to \"console\" or \"telegram\"") env
          putStrLn "Set mode in config to \"console\" or \"telegram\""
      hClose logHandle

handleHttpException :: Environment -> HttpException -> IO ()
handleHttpException env (HttpExceptionRequest _ (ConnectionFailure _)) = do
  runReaderT (printError "Exception thrown: ConnectionFailure \n No connection to server \n Program terminated") env
  putStr "Exception thrown: ConnectionFailure \n No connection to server \n Program terminated"
handleHttpException env (HttpExceptionRequest _ ResponseTimeout) = do
  runReaderT (printError "Exception thrown: ResponseTimeout \n The server took too long to return a response \n Program terminated") env
  putStr "Exception thrown: HttpExceptionRequest \n The server took too long to return a response \n Program terminated"
handleHttpException env (HttpExceptionRequest _ _) = do
  runReaderT (printError "Exception thrown: HttpExceptionRequest \n Bot Token is invalid \n Program terminated") env
  putStr "Exception thrown: HttpExceptionRequest \n Bot Token is invalid \n Program terminated"
handleHttpException env (InvalidUrlException _ _) = do
  runReaderT (printError "Exception thrown: InvalidUrlException \n Program terminated") env
  putStr "Exception thrown: InvalidUrlException \n Program terminated"

handleException :: Environment -> SomeException -> IO ()
handleException env exception = do
  runReaderT (printError $ "Exception thrown: " ++ show exception ++ "\n Program terminated") env
  putStr $ "Exception thrown: " ++ show exception ++ "\n Program terminated"
