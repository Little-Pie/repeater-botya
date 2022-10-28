module Handle where

import Text.Read (readMaybe)

data Result = EchoMessage Int | RepeatMessage | HelpMessage | RepeatNumberSuccess Int | WrongRepeatNumber | WrongRepeatNumberString
  deriving (Eq, Show)

consolBotHandle :: (Monad m) => Bool -> Int -> String -> m (Bool, Result)
consolBotHandle False repNumber "/help" = pure (False, HelpMessage)
consolBotHandle False repNumber "/repeat" = pure (True, RepeatMessage)
consolBotHandle False repNumber str = pure (False, EchoMessage repNumber)
consolBotHandle True repNumber str = do
  let mbNumber = readMaybe str :: Maybe Int
  case mbNumber of
    Just number ->
      if number > 0 && number < 6
        then pure (False, RepeatNumberSuccess number)
        else pure (True, WrongRepeatNumber)
    Nothing -> pure (True, WrongRepeatNumberString)

-- data Handle m = Handle {}
--
-- telegramBotHandle :: (Monad m) => Handle m -> Bool -> Int -> String -> m (Bool, Result)
--
-- telegramBotHandle Handle {..}
