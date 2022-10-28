{-# LANGUAGE RecordWildCards #-}

module ConsolBot where

import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Environment (App, Environment (..))
import Handle as H (Result (..), consolBotHandle)
import Logging (printRelease)
import Text.Read (readMaybe)

consolBotLoop :: App ()
consolBotLoop = do
  Environment {..} <- ask
  consolBot repeatNumber False
  where
    consolBot :: Int -> Bool -> App ()
    consolBot repNumber isRepeat = do
      text <- liftIO getLine
      (newRepNumber, newIsRepeat) <- handlingMessages repNumber isRepeat text
      consolBot newRepNumber newIsRepeat
      pure ()

handlingMessages :: Int -> Bool -> String -> App (Int, Bool)
handlingMessages repNumber isRepeat msg = do
  Environment {..} <- ask
  printRelease $ "[User]: " ++ msg
  (newIsRepeat, res) <- liftIO $ consolBotHandle isRepeat repNumber msg
  case res of
    HelpMessage -> do
      printRelease $ "[Bot]: " ++ helpMessage
      liftIO $ putStrLn helpMessage
      pure (repNumber, newIsRepeat)
    RepeatMessage -> do
      printRelease $ "[Bot]: " ++ repeatMessage
      liftIO $ putStrLn repeatMessage
      pure (repNumber, newIsRepeat)
    EchoMessage echoRepNumber -> do
      replicateM_ echoRepNumber $ printRelease $ "[Bot]: " ++ msg
      liftIO $ replicateM_ echoRepNumber $ putStrLn msg
      pure (echoRepNumber, newIsRepeat)
    RepeatNumberSuccess newRepNumber -> do
      printRelease $ "[Bot]: " ++ repeatAcceptMessage ++ show newRepNumber ++ " times"
      liftIO $ putStrLn $ repeatAcceptMessage ++ show newRepNumber ++ " times"
      pure (newRepNumber, newIsRepeat)
    WrongRepeatNumber -> do
      printRelease $ "[Bot]: " ++ repeatNumberErrorMessage
      liftIO $ putStrLn repeatNumberErrorMessage
      pure (repNumber, newIsRepeat)
    WrongRepeatNumberString -> do
      printRelease $ "[Bot]: " ++ repeatNumberErrorMessage
      liftIO $ putStrLn repeatNumberErrorMessage
      pure (repNumber, newIsRepeat)
