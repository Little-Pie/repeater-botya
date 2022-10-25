{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TelegramBot where

import Config (Config (..))
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Aeson (decodeStrict, encode)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB (toStrict)
import Data.Maybe (fromMaybe)
import Network.HTTP.Client.Internal (RequestBody (RequestBodyBS), ResponseTimeout (ResponseTimeoutMicro))
import Network.HTTP.Simple (addRequestHeader, getResponseBody, httpBS, httpNoBody, parseRequestThrow_, setRequestBody, setRequestMethod, setRequestResponseTimeout)
import Text.Read (readMaybe)
import Types.FromJSON (TelegramUpdates (..), UserMessage (..))
import Types.ToJSON (KeyBoard (..), Keys (..), ReplyMarkup (..))

type RepeatNumbers = [(Int, Int)]

type App a = ReaderT Config IO a

telegramBotLoop :: Int -> [Int] -> RepeatNumbers -> App ()
telegramBotLoop offset chatIdsForRepeat repeatNumbers = do
  telegramResponse <- getUpdates offset
  let mbUpdates = decodeStrict telegramResponse :: Maybe TelegramUpdates
  case mbUpdates of
    Nothing -> liftIO $ putStrLn "Couldn't parse telegramResponse"
    Just updates -> do
      (newOffset, newChatIdsForRepeat, newRepeatNumbers) <- sendMsgs updates chatIdsForRepeat repeatNumbers
      telegramBotLoop newOffset newChatIdsForRepeat newRepeatNumbers

getUpdates :: Int -> App BS.ByteString
getUpdates offset = do
  Config {..} <- ask
  response <- liftIO $ httpBS $ setRequestResponseTimeout (ResponseTimeoutMicro $ (timeout + 1) * 1000000) $ parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/getUpdates?offset=" ++ show offset ++ "&timeout=" ++ show timeout
  return (getResponseBody response)

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
sendTextMsg chatId msg chatIdsForRepeat repeatNumbers = do
  Config {..} <- ask
  replicateM_
    ( fromMaybe
        repeatNumber
        (lookup chatId repeatNumbers)
    )
    $ (liftIO . httpNoBody)
      (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/sendMessage?chat_id=" ++ show chatId ++ "&text=" ++ msg)

sendStickerMsg :: Int -> String -> RepeatNumbers -> App ()
sendStickerMsg chatId stickerId repeatNumbers = do
  Config {..} <- ask
  replicateM_
    ( fromMaybe
        repeatNumber
        (lookup chatId repeatNumbers)
    )
    $ (liftIO . httpNoBody)
      (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/sendSticker?chat_id=" ++ show chatId ++ "&sticker=" ++ stickerId)

sendHelpMsg :: Int -> App ()
sendHelpMsg chatId = do
  Config {..} <- ask
  liftIO $ httpNoBody (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/sendMessage?chat_id=" ++ show chatId ++ "&text=" ++ helpMessage)
  return ()

sendErrorMsg :: Int -> App ()
sendErrorMsg chatId = do
  Config {..} <- ask
  liftIO $ httpNoBody (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/sendMessage?chat_id=" ++ show chatId ++ "&text=" ++ errorMessage)
  return ()

sendRepeatMsg :: Int -> App ()
sendRepeatMsg chatId = do
  config@Config {..} <- ask
  resp <-
    liftIO $
      httpBS $
        addRequestHeader "Content-Type" "application/json" $
          setRequestBody
            (body config)
            ( setRequestMethod "POST" $
                parseRequestThrow_ $
                  "https://api.telegram.org/bot" ++ token ++ "/sendMessage"
            )
  return ()
  where
    body Config {..} = RequestBodyBS $ LB.toStrict $ encode $ KeyBoard chatId repeatMessage (ReplyMarkup [Text "1", Text "2", Text "3", Text "4", Text "5"])

sendRepeatAcceptMsg :: Int -> String -> App ()
sendRepeatAcceptMsg chatId msg = do
  config@Config {..} <- ask
  resp <-
    liftIO $
      httpBS $
        addRequestHeader "Content-Type" "application/json" $
          setRequestBody
            (body config)
            ( setRequestMethod "POST" $
                parseRequestThrow_ $
                  "https://api.telegram.org/bot" ++ token ++ "/sendMessage"
            )
  return ()
  where
    body Config {..} = RequestBodyBS $ LB.toStrict $ encode $ KeyBoard chatId (repeatAcceptMessage ++ msg ++ " times") RemoveKeyboard
