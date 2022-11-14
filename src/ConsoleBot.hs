{-# LANGUAGE RecordWildCards #-}

module ConsoleBot where

import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Reader (ask)
import Environment (App, Environment (..))
import Handle as H (Handle (..), Result (..), messagesHandle)
import Logging (printRelease)

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
  printRelease $ "[User]: " ++ msg
  result <- runMaybeT $ messagesHandle handle isAskedForRepeat repNumber msg
  case result of
    Just (newIsAskedForRepeat, HelpMessage) -> do
      printRelease $ "[Bot]: " ++ helpMessage
      liftIO $ putStrLn helpMessage
      pure (repNumber, newIsAskedForRepeat)
    Just (newIsAskedForRepeat, RepeatMessage) -> do
      printRelease $ "[Bot]: " ++ repeatMessage
      liftIO $ putStrLn repeatMessage
      pure (repNumber, newIsAskedForRepeat)
    Just (newIsAskedForRepeat, EchoMessage echoRepNumber) -> do
      replicateM_ echoRepNumber $ printRelease $ "[Bot]: " ++ msg
      liftIO $ replicateM_ echoRepNumber $ putStrLn msg
      pure (echoRepNumber, newIsAskedForRepeat)
    Just (newIsAskedForRepeat, RepeatNumberSuccess newRepNumber) -> do
      printRelease $ "[Bot]: " ++ repeatAcceptMessage ++ show newRepNumber ++ " times"
      liftIO $ putStrLn $ repeatAcceptMessage ++ show newRepNumber ++ " times"
      pure (newRepNumber, newIsAskedForRepeat)
    Nothing -> do
      printRelease $ "[Bot]: " ++ repeatNumberErrorMessage
      liftIO $ putStrLn repeatNumberErrorMessage
      pure (repNumber, isAskedForRepeat)
  where
    handle =
      Handle
        { getString = Just
        }
