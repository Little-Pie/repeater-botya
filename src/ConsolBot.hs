{-# LANGUAGE RecordWildCards #-}

module ConsolBot where

import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Environment (App, Environment (..))
import Logging (printRelease)
import Text.Read (readMaybe)

consolBotLoop :: App ()
consolBotLoop = do
  Environment {..} <- ask
  consolBot repeatNumber
  where
    consolBot :: Int -> App ()
    consolBot repNumber = do
      text <- liftIO getLine
      newRepNumber <- helper repNumber text
      consolBot newRepNumber
      pure ()

helper :: Int -> String -> App Int
helper repNumber "/help" = do
  Environment {..} <- ask
  printRelease $ "[User]: /help\n[Bot]: " ++ helpMessage
  liftIO $ putStrLn helpMessage
  pure repNumber
helper repNumber "/repeat" = do
  Environment {..} <- ask
  printRelease $ "[User]: /repeat\n[Bot]: " ++ repeatMessage
  liftIO $ putStrLn repeatMessage
  getRepNumber repNumber
helper repNumber str = do
  printRelease $ "[User]: " ++ str ++ concat (replicate repNumber $ "\n[Bot]: " ++ str)
  liftIO $ replicateM_ repNumber (putStrLn str) >> pure repNumber

getRepNumber :: Int -> App Int
getRepNumber repNumber = do
  Environment {..} <- ask
  strNumber <- liftIO getLine
  let mbNumber = readMaybe strNumber :: Maybe Int
  case mbNumber of
    Just number ->
      if number > 0 && number < 6
        then do
          printRelease $ "[User]: " ++ strNumber ++ "\n[Bot]: " ++ repeatAcceptMessage ++ strNumber ++ " times"
          liftIO $ putStrLn $ repeatAcceptMessage ++ strNumber ++ " times"
          pure number
        else do
          printRelease $ "[User]: " ++ strNumber ++ "\n[Bot]: " ++ repeatNumberErrorMessage
          liftIO $ putStrLn repeatNumberErrorMessage
          getRepNumber repNumber
    Nothing -> do
      printRelease $ "[User]: " ++ strNumber ++ "\n[Bot]: " ++ repeatNumberErrorMessage
      liftIO $ putStrLn repeatNumberErrorMessage
      getRepNumber repNumber
