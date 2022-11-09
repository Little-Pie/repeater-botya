{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TelegramBot where

import Control.Monad (replicateM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (decodeStrict, encode)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as LB (toStrict)
import Data.Maybe (fromMaybe)
import Environment (App, Environment (..))
import Handle (Handle (..), Result (..), messagesHandle)
import Logging (printError, printRelease, printWarning)
import Network.HTTP.Client.Internal (RequestBody (RequestBodyBS), ResponseTimeout (ResponseTimeoutMicro))
import Network.HTTP.Simple (addRequestHeader, getResponseBody, httpBS, httpNoBody, parseRequestThrow_, setRequestBody, setRequestMethod, setRequestResponseTimeout)
import Types.FromJSON (TelegramUpdates (..), UserMessage (..))
import Types.ToJSON (KeyBoard (..), Keys (..), ReplyMarkup (..))

type Message = String

type Offset = Int

type ChatId = Int

type RepeatNumber = Int

type RepeatNumbers = [(ChatId, RepeatNumber)]

telegramBotLoop :: Offset -> [ChatId] -> RepeatNumbers -> App ()
telegramBotLoop offset chatIdsForRepeat repeatNumbers = do
  telegramResponse <- getUpdates offset
  let mbUpdates = decodeStrict telegramResponse :: Maybe TelegramUpdates
  case mbUpdates of
    Nothing -> do
      printError "Couldn't parse telegramResponse"
      liftIO $ putStrLn "Couldn't parse telegramResponse"
    Just updates -> do
      (newOffset, newChatIdsForRepeat, newRepeatNumbers) <- sendMsgs updates chatIdsForRepeat repeatNumbers
      telegramBotLoop newOffset newChatIdsForRepeat newRepeatNumbers

getUpdates :: Offset -> App BS.ByteString
getUpdates offset = do
  Environment {..} <- ask
  response <- liftIO $ httpBS $ setRequestResponseTimeout (ResponseTimeoutMicro $ (timeout + 1) * 1000000) $ parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/getUpdates?offset=" ++ show offset ++ "&timeout=" ++ show timeout
  pure (getResponseBody response)

sendMsg :: UserMessage -> RepeatNumber -> App ()
sendMsg userMsg repNumber = do
  Environment {..} <- ask
  case userMsg of
    TextMessage _ chatId msg -> do
      printRelease $ "[User]: " ++ msg
      replicateM_ repNumber $ do
        printRelease $ "[Bot]: " ++ msg
        (liftIO . httpNoBody)
          (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/sendMessage?chat_id=" ++ show chatId ++ "&text=" ++ msg)
    StickerMessage _ chatId stickerId -> do
      printRelease $ "[User]: *some sticker with id " ++ show stickerId ++ "*"
      replicateM_ repNumber $ do
        printRelease $ "[Bot]: *some sticker with id " ++ show stickerId ++ "*"
        (liftIO . httpNoBody)
          (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/sendSticker?chat_id=" ++ show chatId ++ "&sticker=" ++ stickerId)
    NothingMessage _ _ -> do
      printWarning "Warning: User sent unknown type of message"

sendHelpMsg :: ChatId -> App ()
sendHelpMsg chatId = do
  Environment {..} <- ask
  void . liftIO $ httpNoBody (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/sendMessage?chat_id=" ++ show chatId ++ "&text=" ++ helpMessage)

sendRepeatNumberErrorMsg :: ChatId -> App ()
sendRepeatNumberErrorMsg chatId = do
  Environment {..} <- ask
  void . liftIO $ httpNoBody (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/sendMessage?chat_id=" ++ show chatId ++ "&text=" ++ repeatNumberErrorMessage)

sendRepeatMsg :: ChatId -> App ()
sendRepeatMsg chatId = do
  env@Environment {..} <- ask
  void . liftIO $
    httpBS $
      addRequestHeader "Content-Type" "application/json" $
        setRequestBody
          (body env)
          ( setRequestMethod "POST" $
              parseRequestThrow_ $
                "https://api.telegram.org/bot" ++ token ++ "/sendMessage"
          )
  where
    body Environment {..} = RequestBodyBS $ LB.toStrict $ encode $ KeyBoard chatId repeatMessage (ReplyMarkup [Text "1", Text "2", Text "3", Text "4", Text "5"])

sendRepeatAcceptMsg :: ChatId -> Message -> App ()
sendRepeatAcceptMsg chatId msg = do
  env@Environment {..} <- ask
  void . liftIO $
    httpBS $
      addRequestHeader "Content-Type" "application/json" $
        setRequestBody
          (body env)
          ( setRequestMethod "POST" $
              parseRequestThrow_ $
                "https://api.telegram.org/bot" ++ token ++ "/sendMessage"
          )
  where
    body Environment {..} = RequestBodyBS $ LB.toStrict $ encode $ KeyBoard chatId (repeatAcceptMessage ++ msg ++ " times") RemoveKeyboard

sendMsgs :: TelegramUpdates -> [ChatId] -> RepeatNumbers -> App (Offset, [ChatId], RepeatNumbers)
sendMsgs (TelegramUpdates userMessages) chatIdsForRepeat repeatNumbers = do
  Environment {..} <- ask
  case userMessages of
    [] -> pure (0, chatIdsForRepeat, repeatNumbers)
    [userMsg] -> do
      let chatId = getChatId userMsg
      let updateId = getUpdateId userMsg
      let isAskedForRepeat = getChatId userMsg `elem` chatIdsForRepeat
      let repNumber = fromMaybe repeatNumber (lookup chatId repeatNumbers)
      let str = fromMaybe "" (getMessage userMsg)
      (_, res) <- messagesHandle handle isAskedForRepeat repNumber userMsg
      case res of
        HelpMessage -> do
          printRelease $ "[User]: " ++ str ++ "\n [Bot]: " ++ helpMessage
          sendHelpMsg chatId
          pure (updateId + 1, chatIdsForRepeat, repeatNumbers)
        RepeatMessage -> do
          printRelease $ "[User]: " ++ str ++ "\n Bot]: " ++ repeatMessage
          sendRepeatMsg chatId
          pure (updateId + 1, chatId : chatIdsForRepeat, repeatNumbers)
        EchoMessage echoRepNumber -> do
          sendMsg userMsg echoRepNumber
          pure (updateId + 1, chatIdsForRepeat, repeatNumbers)
        RepeatNumberSuccess newRepNumber -> do
          printRelease $ "[User]: " ++ str ++ "\n Bot]: " ++ repeatAcceptMessage ++ show newRepNumber ++ " times"
          sendRepeatAcceptMsg chatId str
          pure (updateId + 1, filter (/= chatId) chatIdsForRepeat, (chatId, read str :: Int) : filter (\a -> fst a /= chatId) repeatNumbers)
        WrongRepeatNumber -> do
          printRelease $ "[User]: " ++ str ++ "\n Bot]: " ++ repeatNumberErrorMessage
          sendRepeatNumberErrorMsg chatId
          pure (updateId + 1, chatIdsForRepeat, repeatNumbers)
    (userMsg : userMsgs) -> do
      let chatId = getChatId userMsg
      let isAskedForRepeat = getChatId userMsg `elem` chatIdsForRepeat
      let repNumber = fromMaybe repeatNumber (lookup chatId repeatNumbers)
      let str = fromMaybe "" (getMessage userMsg)
      (_, res) <- messagesHandle handle isAskedForRepeat repNumber userMsg
      case res of
        HelpMessage -> do
          printRelease $ "[User]: " ++ str ++ "\n [Bot]: " ++ helpMessage
          sendHelpMsg chatId
          sendMsgs (TelegramUpdates userMsgs) chatIdsForRepeat repeatNumbers
        RepeatMessage -> do
          printRelease $ "[User]: " ++ str ++ "\n Bot]: " ++ repeatMessage
          sendRepeatMsg chatId
          sendMsgs (TelegramUpdates userMsgs) (chatId : chatIdsForRepeat) repeatNumbers
        EchoMessage echoRepNumber -> do
          printRelease $ "[User]: " ++ str
          replicateM_ echoRepNumber $ printRelease $ "[Bot]: " ++ str
          sendMsg userMsg echoRepNumber
          sendMsgs (TelegramUpdates userMsgs) chatIdsForRepeat repeatNumbers
        RepeatNumberSuccess newRepNumber -> do
          printRelease $ "[User]: " ++ str ++ "\n Bot]: " ++ repeatAcceptMessage ++ show newRepNumber ++ " times"
          sendRepeatAcceptMsg chatId str
          sendMsgs (TelegramUpdates userMsgs) (filter (/= chatId) chatIdsForRepeat) ((chatId, read str :: Int) : filter (\a -> fst a /= chatId) repeatNumbers)
        WrongRepeatNumber -> do
          printRelease $ "[User]: " ++ str ++ "\n Bot]: " ++ repeatNumberErrorMessage
          sendRepeatNumberErrorMsg chatId
          sendMsgs (TelegramUpdates userMsgs) chatIdsForRepeat repeatNumbers
  where
    handle =
      Handle
        { getString = getMessage
        }

getChatId :: UserMessage -> ChatId
getChatId (TextMessage _ chatId _) = chatId
getChatId (StickerMessage _ chatId _) = chatId
getChatId (NothingMessage _ chatId) = chatId

getUpdateId :: UserMessage -> ChatId
getUpdateId (TextMessage updateId _ _) = updateId
getUpdateId (StickerMessage updateId _ _) = updateId
getUpdateId (NothingMessage updateId _) = updateId

getMessage :: UserMessage -> Maybe Message
getMessage (TextMessage _ _ str) = Just str
getMessage (StickerMessage _ _ str) = Just str
getMessage (NothingMessage _ _) = Nothing
