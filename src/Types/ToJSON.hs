{-# LANGUAGE OverloadedStrings #-}

module Types.ToJSON where

import Data.Aeson (ToJSON (..), object, (.=))

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

newtype Keys = Text' String

instance ToJSON Keys where
  toJSON (Text' str) = object ["text" .= str]
