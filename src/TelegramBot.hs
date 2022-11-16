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
import Environment (App, Environment (..), LoggingLevel (..))
import Handle (Handle (..), Result (..), messagesHandle)
import Logging (printLog)
import Network.HTTP.Client.Internal (RequestBody (RequestBodyBS), ResponseTimeout (ResponseTimeoutMicro))
import Network.HTTP.Simple (addRequestHeader, getResponseBody, httpBS, httpNoBody, parseRequestThrow_, setRequestBody, setRequestMethod, setRequestResponseTimeout)
import Types.Bot (ChatId (..), ChatIdsForRepeat, Message, Offset, RepeatNumber, RepeatNumbersList (..), SendMsgsResult (..), UpdateId (..))
import Types.FromJSON (TelegramUpdates (..), UserMessage (..))
import Types.ToJSON (KeyBoard (..), Keys (..), ReplyMarkup (..))

botTokenCheck :: App ()
botTokenCheck = do
  Environment {..} <- ask
  void $ liftIO $ httpBS $ parseRequestThrow_ $ concat ["https://api.telegram.org/bot", token, "/getMe"]

runTelegramBot :: App ()
runTelegramBot = do
  botTokenCheck
  telegramBotLoop (UpdateId 0) [] (RepeatNumbersList [])

telegramBotLoop :: Offset -> ChatIdsForRepeat -> RepeatNumbersList -> App ()
telegramBotLoop offset chatIdsForRepeat repeatNumbersList = do
  telegramResponse <- getUpdates offset
  let mbUpdates = decodeStrict telegramResponse :: Maybe TelegramUpdates
  case mbUpdates of
    Nothing -> do
      printLog Error "Couldn't parse telegramResponse"
      liftIO $ putStrLn "Couldn't parse telegramResponse"
    Just updates -> do
      SendMsgsResult newOffset newChatIdsForRepeat newRepeatNumbers <- sendMsgs updates chatIdsForRepeat repeatNumbersList
      telegramBotLoop newOffset newChatIdsForRepeat newRepeatNumbers

getUpdates :: Offset -> App BS.ByteString
getUpdates (UpdateId offset) = do
  Environment {..} <- ask
  response <- liftIO $ httpBS $ setRequestResponseTimeout (ResponseTimeoutMicro $ (timeout + 1) * 1000000) $ parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/getUpdates?offset=" ++ show offset ++ "&timeout=" ++ show timeout
  pure (getResponseBody response)

sendMsg :: UserMessage -> RepeatNumber -> App ()
sendMsg userMsg repNumber = do
  Environment {..} <- ask
  case userMsg of
    TextMessage _ (ChatId chatId) msg -> do
      printLog Release $ "[User]: " ++ msg
      replicateM_ repNumber $ do
        printLog Release $ "[Bot]: " ++ msg
        (liftIO . httpNoBody)
          (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/sendMessage?chat_id=" ++ show chatId ++ "&text=" ++ msg)
    StickerMessage _ (ChatId chatId) stickerId -> do
      printLog Release $ "[User]: *some sticker with id " ++ show stickerId ++ "*"
      replicateM_ repNumber $ do
        printLog Release $ "[Bot]: *some sticker with id " ++ show stickerId ++ "*"
        (liftIO . httpNoBody)
          (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/sendSticker?chat_id=" ++ show chatId ++ "&sticker=" ++ stickerId)
    NothingMessage _ _ -> do
      printLog Warning "Warning: User sent unknown type of message"

sendHelpMsg :: ChatId -> App ()
sendHelpMsg (ChatId chatId) = do
  Environment {..} <- ask
  void . liftIO $ httpNoBody (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/sendMessage?chat_id=" ++ show chatId ++ "&text=" ++ helpMessage)

sendRepeatNumberErrorMsg :: ChatId -> App ()
sendRepeatNumberErrorMsg (ChatId chatId) = do
  Environment {..} <- ask
  void . liftIO $ httpNoBody (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/sendMessage?chat_id=" ++ show chatId ++ "&text=" ++ repeatNumberErrorMessage)

sendRepeatMsg :: ChatId -> App ()
sendRepeatMsg (ChatId chatId) = do
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
sendRepeatAcceptMsg (ChatId chatId) msg = do
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

sendMsgs :: TelegramUpdates -> ChatIdsForRepeat -> RepeatNumbersList -> App SendMsgsResult
sendMsgs (TelegramUpdates userMessages) chatIdsForRepeat repeatNumbersList@(RepeatNumbersList repeatNumbers) = do
  Environment {..} <- ask
  case userMessages of
    [] -> pure (SendMsgsResult (UpdateId 0) chatIdsForRepeat repeatNumbersList)
    [userMsg] -> do
      let chatId = getChatId userMsg
      let (UpdateId updateId) = getUpdateId userMsg
      let isAskedForRepeat = getChatId userMsg `elem` chatIdsForRepeat
      let repNumber = fromMaybe repeatNumber (lookup chatId repeatNumbers)
      let str = fromMaybe "" (getMessage userMsg)
      (_, res) <- messagesHandle handle isAskedForRepeat repNumber userMsg
      case res of
        HelpMessage -> do
          printLog Release $ "[User]: " ++ str ++ "\n [Bot]: " ++ helpMessage
          sendHelpMsg chatId
          pure (SendMsgsResult (UpdateId (updateId + 1)) chatIdsForRepeat repeatNumbersList)
        RepeatMessage -> do
          printLog Release $ "[User]: " ++ str ++ "\n [Bot]: " ++ repeatMessage
          sendRepeatMsg chatId
          pure (SendMsgsResult (UpdateId (updateId + 1)) (chatId : chatIdsForRepeat) repeatNumbersList)
        EchoMessage echoRepNumber -> do
          sendMsg userMsg echoRepNumber
          pure (SendMsgsResult (UpdateId (updateId + 1)) chatIdsForRepeat repeatNumbersList)
        RepeatNumberSuccess newRepNumber -> do
          printLog Release $ "[User]: " ++ str ++ "\n [Bot]: " ++ repeatAcceptMessage ++ show newRepNumber ++ " times"
          printLog Debug $ show (RepeatNumbersList ((chatId, read str :: Int) : filter (\a -> fst a /= chatId) repeatNumbers))
          sendRepeatAcceptMsg chatId str
          pure (SendMsgsResult (UpdateId (updateId + 1)) (filter (/= chatId) chatIdsForRepeat) (RepeatNumbersList ((chatId, read str :: Int) : filter (\a -> fst a /= chatId) repeatNumbers)))
        WrongRepeatNumber -> do
          printLog Release $ "[User]: " ++ str ++ "\n Bot]: " ++ repeatNumberErrorMessage
          sendRepeatNumberErrorMsg chatId
          pure (SendMsgsResult (UpdateId (updateId + 1)) chatIdsForRepeat repeatNumbersList)
    (userMsg : userMsgs) -> do
      let chatId = getChatId userMsg
      let isAskedForRepeat = getChatId userMsg `elem` chatIdsForRepeat
      let repNumber = fromMaybe repeatNumber (lookup chatId repeatNumbers)
      let str = fromMaybe "" (getMessage userMsg)
      (_, res) <- messagesHandle handle isAskedForRepeat repNumber userMsg
      case res of
        HelpMessage -> do
          printLog Release $ "[User]: " ++ str ++ "\n [Bot]: " ++ helpMessage
          sendHelpMsg chatId
          sendMsgs (TelegramUpdates userMsgs) chatIdsForRepeat repeatNumbersList
        RepeatMessage -> do
          printLog Release $ "[User]: " ++ str ++ "\n Bot]: " ++ repeatMessage
          sendRepeatMsg chatId
          sendMsgs (TelegramUpdates userMsgs) (chatId : chatIdsForRepeat) repeatNumbersList
        EchoMessage echoRepNumber -> do
          printLog Release $ "[User]: " ++ str
          replicateM_ echoRepNumber $ printLog Release $ "[Bot]: " ++ str
          sendMsg userMsg echoRepNumber
          sendMsgs (TelegramUpdates userMsgs) chatIdsForRepeat repeatNumbersList
        RepeatNumberSuccess newRepNumber -> do
          printLog Release $ "[User]: " ++ str ++ "\n Bot]: " ++ repeatAcceptMessage ++ show newRepNumber ++ " times"
          sendRepeatAcceptMsg chatId str
          sendMsgs (TelegramUpdates userMsgs) (filter (/= chatId) chatIdsForRepeat) (RepeatNumbersList ((chatId, read str :: Int) : filter (\a -> fst a /= chatId) repeatNumbers))
        WrongRepeatNumber -> do
          printLog Release $ "[User]: " ++ str ++ "\n Bot]: " ++ repeatNumberErrorMessage
          sendRepeatNumberErrorMsg chatId
          sendMsgs (TelegramUpdates userMsgs) chatIdsForRepeat repeatNumbersList
  where
    handle =
      Handle
        { getString = getMessage
        }

getChatId :: UserMessage -> ChatId
getChatId (TextMessage _ chatId _) = chatId
getChatId (StickerMessage _ chatId _) = chatId
getChatId (NothingMessage _ chatId) = chatId

getUpdateId :: UserMessage -> UpdateId
getUpdateId (TextMessage updateId _ _) = updateId
getUpdateId (StickerMessage updateId _ _) = updateId
getUpdateId (NothingMessage updateId _) = updateId

getMessage :: UserMessage -> Maybe Message
getMessage (TextMessage _ _ str) = Just str
getMessage (StickerMessage _ _ str) = Just str
getMessage (NothingMessage _ _) = Nothing
