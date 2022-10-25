{-# LANGUAGE RecordWildCards #-}

module ConsolBot where

import Config (Config (..))
import Control.Monad (replicateM_)
import Text.Read (readMaybe)

consolBotLoop :: Config -> IO ()
consolBotLoop Config {..} = do
  consolBot repeatNumber
  where
    consolBot :: Int -> IO ()
    consolBot repeatNumber = do
      text <- getLine
      newRepeatNumber <- helper repeatNumber text
      consolBot newRepeatNumber
      pure ()

helper :: Int -> String -> IO Int
helper repeatNumber "/help" = putStrLn "This bot texts your messages back" >> pure repeatNumber
helper repeatNumber "/repeat" = getRepeatNumber repeatNumber
helper repeatNumber str = replicateM_ repeatNumber (putStrLn str) >> pure repeatNumber

getRepeatNumber repeatNumber = do
  putStrLn "Enter a number of repetition:"
  strNumber <- getLine
  let mbNumber = readMaybe strNumber :: Maybe Int
  case mbNumber of
    Just number -> if number > 0 && number < 6 then pure number else putStrLn msgAboutRepeatNumber >> pure repeatNumber >> getRepeatNumber repeatNumber
    Nothing -> putStrLn msgAboutRepeatNumber >> pure repeatNumber >> getRepeatNumber repeatNumber

msgAboutRepeatNumber = "Please enter number from 1 to 5"
