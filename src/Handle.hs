{-# LANGUAGE RecordWildCards #-}

module Handle where

import Control.Monad (mzero)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Environment (Environment (..))
import Text.Read (readMaybe)

data Handle m msg = Handle
  { getString :: msg -> Maybe String,
    sendMessage :: msg -> Int -> MaybeT (ReaderT Environment m) (),
    sendText :: String -> MaybeT (ReaderT Environment m) (),
    sendRepeat :: String -> MaybeT (ReaderT Environment m) (),
    sendRepeatAccept :: String -> String -> MaybeT (ReaderT Environment m) ()
  }

messagesHandle :: (Monad m) => Handle m msg -> Bool -> Int -> msg -> MaybeT (ReaderT Environment m) (Bool, Int)
messagesHandle Handle {..} isAskedForRepeat repNumber msg
  | isAskedForRepeat = do
    Environment {..} <- lift ask
    res <- maybe (sendText repeatNumberErrorMessage >> mzero) pure (getString msg)
    number <- maybe (sendText repeatNumberErrorMessage >> mzero) pure (readMaybe res)
    if number > 0 && number < 6
      then sendRepeatAccept repeatAcceptMessage res >> pure (False, number)
      else sendText repeatNumberErrorMessage >> mzero
  | otherwise = do
    Environment {..} <- lift ask
    case getString msg of
      Just "/help" -> sendText helpMessage >> pure (False, repNumber)
      Just "/repeat" -> sendRepeat repeatMessage >> pure (True, repNumber)
      _ -> sendMessage msg repNumber >> pure (False, repNumber)
