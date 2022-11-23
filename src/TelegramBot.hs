{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TelegramBot where

import Control.Monad (foldM, replicateM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (decodeStrict, encode)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BSC (pack)
import qualified Data.ByteString.Lazy as LB (toStrict)
import qualified Data.Map.Internal as Map
  ( empty,
    insert,
    lookup,
  )
import Data.Maybe (fromMaybe)
import Environment (App, Environment (..), LoggingLevel (..))
import Handle (Handle (..), messagesHandle)
import Logging (printLog)
import Network.HTTP.Client.Internal
  ( RequestBody (RequestBodyBS),
    ResponseTimeout (ResponseTimeoutMicro),
  )
import Network.HTTP.Simple
  ( addRequestHeader,
    getResponseBody,
    httpBS,
    httpNoBody,
    parseRequestThrow_,
    setRequestBody,
    setRequestMethod,
    setRequestQueryString,
    setRequestResponseTimeout,
  )
import Types.Bot
  ( ChatId (..),
    ChatIdsForRepeat,
    Message,
    Offset,
    RepeatNumber,
    RepeatNumbersList,
    SendMsgsResult (..),
    UpdateId (..),
  )
import Types.FromJSON (TelegramUpdates (..), UserMessage (..))
import Types.ToJSON (KeyBoard (..), Keys (..), ReplyMarkup (..))

telegramUrl :: String
telegramUrl = "https://api.telegram.org/bot"

justBS :: String -> Maybe BS.ByteString
justBS = Just . BSC.pack

botTokenCheck :: App ()
botTokenCheck = do
  Environment {..} <- ask
  void $ liftIO $ httpBS $ parseRequestThrow_ $ concat [telegramUrl, token, "/getMe"]

runTelegramBot :: App ()
runTelegramBot = do
  botTokenCheck
  telegramBotLoop (UpdateId 0) [] Map.empty

telegramBotLoop :: Offset -> ChatIdsForRepeat -> RepeatNumbersList -> App ()
telegramBotLoop offset chatIdsForRepeat repeatNumbersList = do
  telegramResponse <- getUpdates offset
  let mbUpdates = decodeStrict telegramResponse :: Maybe TelegramUpdates
  case mbUpdates of
    Nothing -> do
      printLog Error "Couldn't parse telegramResponse"
      liftIO $ putStrLn "Couldn't parse telegramResponse"
    Just updates -> do
      SendMsgsResult (UpdateId newOffset) newChatIdsForRepeat newRepeatNumbers <- sendMsgs updates chatIdsForRepeat repeatNumbersList
      telegramBotLoop (UpdateId $ newOffset + 1) newChatIdsForRepeat newRepeatNumbers

getUpdates :: Offset -> App BS.ByteString
getUpdates (UpdateId offset) = do
  Environment {..} <- ask
  response <-
    liftIO $
      httpBS $
        setRequestResponseTimeout
          (ResponseTimeoutMicro $ (timeout + 1) * 1000000)
          $ setRequestQueryString
            [("offset", justBS $ show offset), ("timeout", justBS $ show timeout)]
            $ parseRequestThrow_ $
              concat
                [telegramUrl, token, "/getUpdates"]
  pure (getResponseBody response)

sendMsg :: UserMessage -> RepeatNumber -> App ()
sendMsg userMsg repNumber = do
  Environment {..} <- ask
  case userMsg of
    TextMessage _ (ChatId chatId) msg -> do
      printLog Release ["[User]: " ++ msg]
      replicateM_ repNumber $ do
        printLog Release ["[Bot]: " ++ msg]
        (liftIO . httpNoBody) $
          setRequestQueryString
            [("chat_id", justBS $ show chatId), ("text", justBS msg)]
            $ parseRequestThrow_ $
              concat
                [telegramUrl, token, "/sendMessage"]
    StickerMessage _ (ChatId chatId) stickerId -> do
      printLog Release $ concat ["[User]: *some sticker with id ", stickerId, "*"]
      replicateM_ repNumber $ do
        printLog Release $ concat ["[Bot]: *some sticker with id ", stickerId, "*"]
        (liftIO . httpNoBody) $
          setRequestQueryString
            [("chat_id", justBS $ show chatId), ("sticker", justBS stickerId)]
            $ parseRequestThrow_ $
              concat
                [telegramUrl, token, "/sendSticker"]
    NothingMessage _ _ -> do
      printLog Warning "Warning: User sent unknown type of message"

sendHelpMsg :: ChatId -> App ()
sendHelpMsg (ChatId chatId) = do
  Environment {..} <- ask
  void . liftIO $
    httpNoBody $
      setRequestQueryString
        [("chat_id", justBS $ show chatId), ("text", justBS helpMessage)]
        $ parseRequestThrow_ $
          concat
            [telegramUrl, token, "/sendMessage"]

sendRepeatNumberErrorMsg :: String -> ChatId -> App ()
sendRepeatNumberErrorMsg str (ChatId chatId) = do
  Environment {..} <- ask
  void . liftIO $
    httpNoBody $
      setRequestQueryString
        [("chat_id", justBS $ show chatId), ("text", justBS str)]
        $ parseRequestThrow_ $
          concat
            [telegramUrl, token, "/sendMessage"]

sendRepeatMsg :: ChatId -> String -> App ()
sendRepeatMsg (ChatId chatId) str = do
  Environment {..} <- ask
  void . liftIO $
    httpBS $
      addRequestHeader "Content-Type" "application/json" $
        setRequestBody
          body
          ( setRequestMethod "POST" $
              parseRequestThrow_ $
                concat [telegramUrl, token, "/sendMessage"]
          )
  where
    body =
      RequestBodyBS $
        LB.toStrict $
          encode $
            KeyBoard
              chatId
              str
              (ReplyMarkup [Text "1", Text "2", Text "3", Text "4", Text "5"])

sendRepeatAcceptMsg :: ChatId -> String -> Message -> App ()
sendRepeatAcceptMsg (ChatId chatId) str msg = do
  Environment {..} <- ask
  void . liftIO $
    httpBS $
      addRequestHeader "Content-Type" "application/json" $
        setRequestBody
          body
          ( setRequestMethod "POST" $
              parseRequestThrow_ $
                concat [telegramUrl, token, "/sendMessage"]
          )
  where
    body =
      RequestBodyBS $
        LB.toStrict $
          encode $
            KeyBoard chatId (concat [str, msg, " times"]) RemoveKeyboard

sendMsgs :: TelegramUpdates -> ChatIdsForRepeat -> RepeatNumbersList -> App SendMsgsResult
sendMsgs (TelegramUpdates userMessages) chatIdsForRepeat repeatNumbersList = do
  Environment {..} <- ask
  foldM
    ( \(SendMsgsResult _ chatIdsForRepeatAcc repeatNumbersAcc) userMsg -> do
        let chatId = getChatId userMsg
        let updateId = getUpdateId userMsg
        let isAskedForRepeat = getChatId userMsg `elem` chatIdsForRepeat
        let repNumber = fromMaybe repeatNumber (Map.lookup chatId repeatNumbersList)
        let str = fromMaybe "" (getMessage userMsg)
        result <- runMaybeT (messagesHandle (handle chatId str) isAskedForRepeat repNumber userMsg)
        case result of
          Just (True, _newRepNumber) ->
            pure (SendMsgsResult updateId (chatId : chatIdsForRepeatAcc) repeatNumbersAcc)
          Just (False, newRepNumber) ->
            pure
              ( SendMsgsResult
                  updateId
                  (filter (/= chatId) chatIdsForRepeatAcc)
                  (Map.insert chatId newRepNumber repeatNumbersAcc)
              )
          Nothing -> pure (SendMsgsResult updateId chatIdsForRepeatAcc repeatNumbersAcc)
    )
    (SendMsgsResult (UpdateId 0) chatIdsForRepeat repeatNumbersList)
    userMessages
  where
    handle chatId str =
      Handle
        { getString = getMessage,
          sendMessage = \message echoNum -> lift $ sendMsg message echoNum,
          sendText = \text -> do
            lift $
              printLog Release $
                concat
                  ["[User]: ", str, "\n[Bot]: ", text]
            lift $ sendRepeatNumberErrorMsg text chatId,
          sendRepeat = \text -> do
            lift $
              printLog Release $
                concat
                  ["[User]: ", str, "\n[Bot]: ", text]
            lift $ sendRepeatMsg chatId text,
          sendRepeatAccept = \text newNum -> do
            lift $
              printLog Release $
                concat
                  ["[User]: ", newNum, "\n[Bot]: ", text, newNum, " times"]
            lift $ sendRepeatAcceptMsg chatId text newNum
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
