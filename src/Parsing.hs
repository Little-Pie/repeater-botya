{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import Data.Aeson
import Control.Monad (mzero)
import Control.Applicative

data TelegramUpdates = TelegramUpdates { updates :: [UserMessage] }

instance FromJSON TelegramUpdates where
    parseJSON (Object telegramUpdates) = do
        anArray <- telegramUpdates .: "result"
        return $ TelegramUpdates anArray
    parseJSON _             = mzero

data UserMessage = TextMessage Int Int String
                 | StickerMessage Int Int String

instance FromJSON UserMessage where
    parseJSON (Object userMessage) = (TextMessage <$> (userMessage .: "update_id")
                                     <*> (userMessage .: "message" >>= (.: "from") >>= (.: "id"))
                                     <*> (userMessage .: "message" >>= (.: "text"))) <|>
                                     (StickerMessage <$> (userMessage .: "update_id")
                                     <*> (userMessage .: "message" >>= (.: "from") >>= (.: "id"))
                                     <*> (userMessage .: "message" >>= (.: "sticker") >>= (.: "file_id")))
    parseJSON _             = mzero
