{-# LANGUAGE RecordWildCards #-}

module ConsoleBot where

import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Reader (ask)
import Environment (App, Environment (..), LoggingLevel (..))
import Handle as H (Handle (..), Result (..), messagesHandle)
import Logging (printLog)

consoleBotLoop :: App ()
consoleBotLoop = do
  Environment {..} <- ask
  consoleBot repeatNumber False
  where
    consoleBot :: Int -> Bool -> App ()
    consoleBot repNumber isAskedForRepeat = do
      text <- liftIO getLine
      (newRepNumber, newIsAskedForRepeat) <- handlingMessages repNumber isAskedForRepeat text
      consoleBot newRepNumber newIsAskedForRepeat
      pure ()

handlingMessages :: Int -> Bool -> String -> App (Int, Bool)
handlingMessages repNumber isAskedForRepeat msg = do
  Environment {..} <- ask
  printLog Release $ "[User]: " ++ msg
  result <- runMaybeT $ messagesHandle handle isAskedForRepeat repNumber msg
  case result of
    Just (newIsAskedForRepeat, HelpMessage) -> do
      printLog Release $ "[Bot]: " ++ helpMessage
      liftIO $ putStrLn helpMessage
      pure (repNumber, newIsAskedForRepeat)
    Just (newIsAskedForRepeat, RepeatMessage) -> do
      printLog Release $ "[Bot]: " ++ repeatMessage
      liftIO $ putStrLn repeatMessage
      pure (repNumber, newIsAskedForRepeat)
    Just (newIsAskedForRepeat, EchoMessage echoRepNumber) -> do
      replicateM_ echoRepNumber $ printLog Release $ "[Bot]: " ++ msg
      liftIO $ replicateM_ echoRepNumber $ putStrLn msg
      pure (echoRepNumber, newIsAskedForRepeat)
    Just (newIsAskedForRepeat, RepeatNumberSuccess newRepNumber) -> do
      printLog Release $ "[Bot]: " ++ repeatAcceptMessage ++ show newRepNumber ++ " times"
      liftIO $ putStrLn $ repeatAcceptMessage ++ show newRepNumber ++ " times"
      pure (newRepNumber, newIsAskedForRepeat)
    Nothing -> do
      printLog Release $ "[Bot]: " ++ repeatNumberErrorMessage
      liftIO $ putStrLn repeatNumberErrorMessage
      pure (repNumber, isAskedForRepeat)
  where
    handle =
      Handle
        { getString = Just
        }
