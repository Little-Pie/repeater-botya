{-# LANGUAGE OverloadedStrings #-}

module Types.FromJSON where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), Value (..), (.:))
import Types.Bot (ChatId, Message, UpdateId)

newtype TelegramUpdates = TelegramUpdates {updates :: [UserMessage]}

instance FromJSON TelegramUpdates where
  parseJSON (Object telegramUpdates) = do
    anArray <- telegramUpdates .: "result"
    pure $ TelegramUpdates anArray
  parseJSON _ = mzero

data UserMessage
  = TextMessage UpdateId ChatId Message
  | StickerMessage UpdateId ChatId Message
  | NothingMessage UpdateId ChatId

instance FromJSON UserMessage where
  parseJSON (Object userMessage) =
    ( TextMessage <$> (userMessage .: "update_id")
        <*> (userMessage .: "message" >>= (.: "from") >>= (.: "id"))
        <*> (userMessage .: "message" >>= (.: "text"))
    )
      <|> ( StickerMessage <$> (userMessage .: "update_id")
              <*> (userMessage .: "message" >>= (.: "from") >>= (.: "id"))
          )
        <*> (userMessage .: "message" >>= (.: "sticker") >>= (.: "file_id"))
      <|> ( NothingMessage <$> (userMessage .: "update_id")
              <*> (userMessage .: "message" >>= (.: "from") >>= (.: "id"))
          )
  parseJSON _ = mzero
