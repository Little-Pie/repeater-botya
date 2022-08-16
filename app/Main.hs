module Main where

import TelegramBot
import ConsolBot
import Parsing
import Control.Monad.Trans.Reader (runReaderT)

main :: IO ()
main = do
  mbConfig <- getConfig
  case mbConfig of
    Nothing -> putStrLn "Couldn't parse config"
    Just config  -> case mode config of
      "telegram" -> runReaderT (botLoop 0 [] []) config
      "consol" -> consolBotLoop
      _ -> putStrLn "Set mode in config to \"consol\" or \"telegram\""
