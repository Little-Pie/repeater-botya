{-# LANGUAGE RecordWildCards #-}

module Handle where

import Control.Monad (mzero)
import Control.Monad.Trans.Maybe (MaybeT)
import Text.Read (readMaybe)

newtype Handle msg = Handle
  { getString :: msg -> Maybe String
  }

data Result
  = EchoMessage Int
  | RepeatMessage
  | HelpMessage
  | RepeatNumberSuccess Int
  deriving (Eq, Show)

messagesHandle :: (Monad m) => Handle msg -> Bool -> Int -> msg -> MaybeT m (Bool, Result)
messagesHandle Handle {..} isAskedForRepeat repNumber msg
  | isAskedForRepeat = do
    res <- maybe mzero pure (getString msg)
    number <- maybe mzero pure (readMaybe res)
    if number > 0 && number < 6
      then pure (False, RepeatNumberSuccess number)
      else mzero
  | otherwise = case getString msg of
    Just "/help" -> pure (False, HelpMessage)
    Just "/repeat" -> pure (True, RepeatMessage)
    _ -> pure (False, EchoMessage repNumber)
