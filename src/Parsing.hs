{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import Data.Aeson
import Control.Monad (mzero)
import Control.Applicative ((<|>))

data TelegramUpdates = TelegramUpdates { updates :: [UserMessage] }

data Config = Config {token :: String
                     ,timeout :: Int
                     ,helpMessage :: String
                     ,repeatMessage :: String
                     ,repeatNumber :: Int
                     ,repeatAcceptMessage :: String
                     ,errorMessage :: String
                     ,mode :: String}

instance FromJSON Config where
  parseJSON (Object config) = Config <$>
                                config .: "token" <*>
                                config .: "timeout" <*>
                                config .: "helpMessage" <*>
                                config .: "repeatMessage" <*>
                                config .: "repeatNumber" <*>
                                config .: "repeatAcceptMessage" <*>
                                config .: "errorMessage" <*>
                                config .: "mode"
  parseJSON _               = mzero

instance FromJSON TelegramUpdates where
    parseJSON (Object telegramUpdates) = do
        anArray <- telegramUpdates .: "result"
        return $ TelegramUpdates anArray
    parseJSON _             = mzero

data UserMessage = TextMessage Int Int String
                 | StickerMessage Int Int String
                 | NothingMessage Int

instance FromJSON UserMessage where
    parseJSON (Object userMessage) = (TextMessage <$> (userMessage .: "update_id")
                                     <*> (userMessage .: "message" >>= (.: "from") >>= (.: "id"))
                                     <*> (userMessage .: "message" >>= (.: "text"))) <|>
                                     (StickerMessage <$> (userMessage .: "update_id")
                                     <*> (userMessage .: "message" >>= (.: "from") >>= (.: "id"))
                                     <*> (userMessage .: "message" >>= (.: "sticker") >>= (.: "file_id"))) <|>
                                     (NothingMessage <$> (userMessage .: "update_id"))
    parseJSON _             = mzero
