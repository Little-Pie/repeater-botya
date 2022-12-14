{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Bot where

import Data.Aeson (FromJSON)
import Data.Map.Internal (Map)
import GHC.Generics (Generic)

newtype ChatId = ChatId Int deriving (Eq, Show, Ord, Generic, FromJSON)

type ChatIdsForRepeat = [ChatId]

newtype UpdateId = UpdateId Int deriving (Eq, Show, Generic, FromJSON)

type Offset = UpdateId

type Message = String

type RepeatNumber = Int

type RepeatNumbersList = Map ChatId RepeatNumber

data SendMsgsResult = SendMsgsResult
  { offsetRes :: Offset,
    chatIdsForRepeatRes :: ChatIdsForRepeat,
    repeatNumbersRes :: RepeatNumbersList
  }
