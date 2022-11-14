{-# LANGUAGE RecordWildCards #-}

module Handle where

import Text.Read (readMaybe)

newtype Handle msg = Handle
  { getString :: msg -> Maybe String
  }

data Result
  = EchoMessage Int
  | RepeatMessage
  | HelpMessage
  | RepeatNumberSuccess Int
  | WrongRepeatNumber
  deriving (Eq, Show)

messagesHandle :: (Monad m) => Handle msg -> Bool -> Int -> msg -> m (Bool, Result)
messagesHandle Handle {..} isAskedForRepeat repNumber msg = do
  if isAskedForRepeat
    then do
      case getString msg of
        Just str -> do
          let mbNumber = readMaybe str :: Maybe Int
          case mbNumber of
            Just number ->
              if number > 0 && number < 6
                then pure (False, RepeatNumberSuccess number)
                else pure (True, WrongRepeatNumber)
            Nothing -> pure (True, WrongRepeatNumber)
        Nothing -> pure (True, WrongRepeatNumber)
    else case getString msg of
      Just "/help" -> pure (False, HelpMessage)
      Just "/repeat" -> pure (True, RepeatMessage)
      _ -> pure (False, EchoMessage repNumber)
