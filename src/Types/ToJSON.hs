{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types.ToJSON where

import Data.Aeson (ToJSON (..), object, (.=))

data KeyBoard = KeyBoard
  { chatId :: Int,
    text :: String,
    replyMarkup :: ReplyMarkup
  }

instance ToJSON KeyBoard where
  toJSON KeyBoard {..} =
    object
      [ "chat_id" .= chatId,
        "text" .= text,
        "reply_markup" .= replyMarkup
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

newtype Keys = Text String

instance ToJSON Keys where
  toJSON (Text str) = object ["text" .= str]
