{-# LANGUAGE RecordWildCards #-}

module ConsoleBot where

import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Reader (ask)
import Environment (App, Environment (..), LoggingLevel (..))
import Handle as H (Handle (..), messagesHandle)
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
  printLog Release $ "[User]: " ++ msg
  result <- runMaybeT $ messagesHandle handle isAskedForRepeat repNumber msg
  case result of
    Just (newIsAskedForRepeat, newRepNumber) ->
      pure (newRepNumber, newIsAskedForRepeat)
    Nothing ->
      pure (repNumber, isAskedForRepeat)
  where
    handle =
      Handle
        { getString = Just,
          sendMessage = \message echoNum -> do
            replicateM_ echoNum $ lift . printLog Release $ "[Bot]: " ++ message
            liftIO $ replicateM_ echoNum $ putStrLn message,
          sendText = \str -> do
            lift . printLog Release $ "[Bot]: " ++ str
            liftIO $ putStrLn str,
          sendRepeat = \str -> do
            lift . printLog Release $ "[Bot]: " ++ str
            liftIO $ putStrLn str,
          sendRepeatAccept = \str newNum -> do
            lift . printLog Release $ "[Bot]: " ++ str ++ newNum ++ " times"
            liftIO $ putStrLn $ str ++ newNum ++ " times"
        }
