{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson

newtype TelegramUpdates = TelegramUpdates {updates :: [UserMessage]}

data KeyBoard = KeyBoard
  { chat_id' :: Int,
    text' :: String,
    reply_markup' :: ReplyMarkup
  }

instance ToJSON KeyBoard where
  toJSON (KeyBoard aChatid aText aReplymarkup) =
    object
      [ "chat_id" .= aChatid,
        "text" .= aText,
        "reply_markup" .= aReplymarkup
      ]

data ReplyMarkup
  = ReplyMarkup [Keys]
  | RemoveKeyboard

instance ToJSON ReplyMarkup where
  toJSON (ReplyMarkup keys) =
    object
      [ "keyboard" .= [keys],
        "one_time_keyboard" .= True
      ]
  toJSON RemoveKeyboard = object ["remove_keyboard" .= True]

data Keys = Text' String

instance ToJSON Keys where
  toJSON (Text' str) = object ["text" .= str]

instance FromJSON TelegramUpdates where
  parseJSON (Object telegramUpdates) = do
    anArray <- telegramUpdates .: "result"
    return $ TelegramUpdates anArray
  parseJSON _ = mzero

data UserMessage
  = TextMessage Int Int String
  | StickerMessage Int Int String
  | NothingMessage Int

instance FromJSON UserMessage where
  parseJSON (Object userMessage) =
    ( TextMessage <$> (userMessage .: "update_id")
        <*> (userMessage .: "message" >>= (.: "from") >>= (.: "id"))
        <*> (userMessage .: "message" >>= (.: "text"))
    )
      <|> ( StickerMessage <$> (userMessage .: "update_id")
              <*> (userMessage .: "message" >>= (.: "from") >>= (.: "id"))
              <*> (userMessage .: "message" >>= (.: "sticker") >>= (.: "file_id"))
          )
      <|> (NothingMessage <$> (userMessage .: "update_id"))
  parseJSON _ = mzero
