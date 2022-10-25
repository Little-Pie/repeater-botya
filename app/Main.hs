module Main where

import Config (Config (..), getConfig)
import ConsolBot (consolBotLoop)
import Control.Monad.Trans.Reader (runReaderT)
import TelegramBot (telegramBotLoop)

main :: IO ()
main = do
  mbConfig <- getConfig
  case mbConfig of
    Nothing -> putStrLn "Couldn't parse config"
    Just config -> case mode config of
      "telegram" -> runReaderT (telegramBotLoop 0 [] []) config
      "consol" -> consolBotLoop
      _ -> putStrLn "Set mode in config to \"consol\" or \"telegram\""
