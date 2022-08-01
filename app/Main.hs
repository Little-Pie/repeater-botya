module Main where

import TelegramBot
import ConsolBot
import Parsing

main :: IO ()
main = do
  mbConfig <- getConfig
  case mbConfig of
    Nothing -> putStrLn "Couldn't parse config"
    Just fromConfig  -> case mode fromConfig of
      "telegram" -> botLoop 0 fromConfig [] []
      "consol" -> consolBotLoop
      _ -> putStrLn "Set mode in config to \"consol\" or \"telegram\""
