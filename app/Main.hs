module Main where

import ConsolBot
import Control.Monad.Trans.Reader (runReaderT)
import Parsing
import TelegramBot

main :: IO ()
main = do
  mbConfig <- getConfig
  case mbConfig of
    Nothing -> putStrLn "Couldn't parse config"
    Just config -> case mode config of
      "telegram" -> runReaderT (botLoop 0 [] []) config
      "consol" -> consolBotLoop
      _ -> putStrLn "Set mode in config to \"consol\" or \"telegram\""
