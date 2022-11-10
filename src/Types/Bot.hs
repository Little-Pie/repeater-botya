module Types.Bot where

type ChatId = Int

type ChatIdsForRepeat = [ChatId]

type Message = String

type Offset = Int

type UpdateId = Int

type RepeatNumber = Int

type RepeatNumbers = (ChatId, RepeatNumber)

data SendMsgsResult = SendMsgsResult
  { offsetRes :: Offset,
    chatIdsForRepeatRes :: ChatIdsForRepeat,
    repeatNumbersRes :: RepeatNumbersList
  }

newtype RepeatNumbersList = RepeatNumbersList [RepeatNumbers]
  deriving (Show)
