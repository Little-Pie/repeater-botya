{-# LANGUAGE OverloadedStrings #-}

module TelegramBot where

import Config (Config (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Aeson (decodeStrict, encode)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB (toStrict)
import Network.HTTP.Client.Internal (RequestBody (RequestBodyBS), ResponseTimeout (ResponseTimeoutMicro))
import Network.HTTP.Simple
import Text.Read (readMaybe)
import Types.FromJSON (TelegramUpdates (..), UserMessage (..))
import Types.ToJSON (KeyBoard (..), Keys (..), ReplyMarkup (..))

type RepeatNumbers = [(Int, Int)]

type App a = ReaderT Config IO a

telegramBotLoop :: Int -> [Int] -> RepeatNumbers -> App ()
telegramBotLoop offset chatIdsForRepeat repeatNumbers = do
  telegramResponse <- getUpdates offset
  let mbUpdates = parseUpdates telegramResponse
  case mbUpdates of
    Nothing -> liftIO $ putStrLn "Couldn't parse telegramResponse"
    Just updates -> do
      (newOffset, newChatIdsForRepeat, newRepeatNumbers) <- sendMsgs updates chatIdsForRepeat repeatNumbers
      telegramBotLoop newOffset newChatIdsForRepeat newRepeatNumbers

getUpdates :: Int -> App B.ByteString
getUpdates offset = do
  config <- ask
  response <- liftIO $ httpBS $ setRequestResponseTimeout (ResponseTimeoutMicro $ (timeout config + 1) * 1000000) $ parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token config ++ "/getUpdates?offset=" ++ show offset ++ "&timeout=" ++ show (timeout config)
  return (getResponseBody response)

parseUpdates :: B.ByteString -> Maybe TelegramUpdates
parseUpdates = decodeStrict

sendMsgs :: TelegramUpdates -> [Int] -> RepeatNumbers -> App (Int, [Int], RepeatNumbers)
sendMsgs (TelegramUpdates userMessages) chatIdsForRepeat repeatNumbers = do
  case userMessages of
    [] -> return (0, chatIdsForRepeat, repeatNumbers)
    [x] -> case x of
      TextMessage updateId chatId msg -> case msg of
        "/help" -> do
          sendHelpMsg chatId
          return (updateId + 1, chatIdsForRepeat, repeatNumbers)
        "/repeat" -> do
          sendRepeatMsg chatId
          return (updateId + 1, chatId : chatIdsForRepeat, repeatNumbers)
        _ ->
          if chatId `notElem` chatIdsForRepeat
            then do
              sendTextMsg chatId msg chatIdsForRepeat repeatNumbers
              return (updateId + 1, chatIdsForRepeat, repeatNumbers)
            else do
              case readMaybe msg :: Maybe Int of
                Nothing -> do
                  sendErrorMsg chatId
                  sendRepeatMsg chatId
                  return (updateId + 1, chatIdsForRepeat, repeatNumbers)
                Just numb -> do
                  if numb > 0 && numb < 6
                    then do
                      sendRepeatAcceptMsg chatId msg
                      return (updateId + 1, filter (/= chatId) chatIdsForRepeat, (chatId, read msg :: Int) : repeatNumbers)
                    else do
                      sendErrorMsg chatId
                      sendRepeatMsg chatId
                      return (updateId + 1, chatIdsForRepeat, repeatNumbers)
      StickerMessage updateId chatId fileId -> do
        sendStickerMsg chatId fileId repeatNumbers
        return (updateId + 1, chatIdsForRepeat, repeatNumbers)
      NothingMessage updateId -> return (updateId + 1, chatIdsForRepeat, repeatNumbers)
    (x : xs) -> case x of
      TextMessage updateId chatId msg -> case msg of
        "/help" -> do
          sendHelpMsg chatId
          sendMsgs (TelegramUpdates xs) chatIdsForRepeat repeatNumbers
        "/repeat" -> do
          sendRepeatMsg chatId
          sendMsgs (TelegramUpdates xs) (chatId : chatIdsForRepeat) repeatNumbers
        _ ->
          if chatId `notElem` chatIdsForRepeat
            then do
              sendTextMsg chatId msg chatIdsForRepeat repeatNumbers
              sendMsgs (TelegramUpdates xs) chatIdsForRepeat repeatNumbers
            else do
              sendRepeatAcceptMsg chatId msg
              sendMsgs (TelegramUpdates xs) (filter (/= chatId) chatIdsForRepeat) ((chatId, read msg :: Int) : repeatNumbers)
      StickerMessage updateId chatId fileId -> do
        sendStickerMsg chatId fileId repeatNumbers
        sendMsgs (TelegramUpdates xs) chatIdsForRepeat repeatNumbers
      NothingMessage updateId -> sendMsgs (TelegramUpdates xs) chatIdsForRepeat repeatNumbers

sendTextMsg :: Int -> String -> [Int] -> RepeatNumbers -> App ()
sendTextMsg chatid msg chatIdsForRepeat repeatNumbers = do
  config <- ask
  mapM (liftIO . httpNoBody) $
    replicate
      ( case lookup chatid repeatNumbers of
          Nothing -> repeatNumber config
          Just repeatNumb -> repeatNumb
      )
      $ (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token config ++ "/sendMessage?chat_id=" ++ show chatid ++ "&text=" ++ msg)
  return ()

sendStickerMsg :: Int -> String -> RepeatNumbers -> App ()
sendStickerMsg chatid stickerID repeatNumbers = do
  config <- ask
  mapM (liftIO . httpNoBody) $
    replicate
      ( case lookup chatid repeatNumbers of
          Nothing -> repeatNumber config
          Just repeatNumb -> repeatNumb
      )
      $ (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token config ++ "/sendSticker?chat_id=" ++ show chatid ++ "&sticker=" ++ stickerID)
  return ()

sendHelpMsg :: Int -> App ()
sendHelpMsg chatid = do
  config <- ask
  liftIO $ httpNoBody (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token config ++ "/sendMessage?chat_id=" ++ show chatid ++ "&text=" ++ helpMessage config)
  return ()

sendErrorMsg :: Int -> App ()
sendErrorMsg chatid = do
  config <- ask
  liftIO $ httpNoBody (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token config ++ "/sendMessage?chat_id=" ++ show chatid ++ "&text=" ++ errorMessage config)
  return ()

sendRepeatMsg :: Int -> App ()
sendRepeatMsg chatid = do
  config <- ask
  resp <-
    liftIO $
      httpBS $
        addRequestHeader "Content-Type" "application/json" $
          setRequestBody
            (body config)
            ( setRequestMethod "POST" $
                parseRequestThrow_ $
                  "https://api.telegram.org/bot" ++ token config ++ "/sendMessage"
            )
  return ()
  where
    body config = RequestBodyBS $ LB.toStrict $ encode $ KeyBoard chatid (repeatMessage config) (ReplyMarkup [Text' "1", Text' "2", Text' "3", Text' "4", Text' "5"])

sendRepeatAcceptMsg :: Int -> String -> App ()
sendRepeatAcceptMsg chatid msg = do
  config <- ask
  resp <-
    liftIO $
      httpBS $
        addRequestHeader "Content-Type" "application/json" $
          setRequestBody
            (body config)
            ( setRequestMethod "POST" $
                parseRequestThrow_ $
                  "https://api.telegram.org/bot" ++ token config ++ "/sendMessage"
            )
  return ()
  where
    body config = RequestBodyBS $ LB.toStrict $ encode $ KeyBoard chatid (repeatAcceptMessage config ++ msg ++ " times") RemoveKeyboard
