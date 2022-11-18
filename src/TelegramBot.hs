{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TelegramBot where

import Control.Monad (foldM, replicateM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (decodeStrict, encode)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BSC (pack)
import qualified Data.ByteString.Lazy as LB (toStrict)
import qualified Data.Map.Internal as Map
  ( empty,
    filterWithKey,
    insert,
    lookup,
  )
import Data.Maybe (fromMaybe)
import Environment (App, Environment (..), LoggingLevel (..))
import Handle (Handle (..), Result (..), messagesHandle)
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
      printLog Release $ concat ["[User]: ", msg]
      replicateM_ repNumber $ do
        printLog Release $ concat ["[Bot]: ", msg]
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

sendRepeatNumberErrorMsg :: ChatId -> App ()
sendRepeatNumberErrorMsg (ChatId chatId) = do
  Environment {..} <- ask
  void . liftIO $
    httpNoBody $
      setRequestQueryString
        [("chat_id", justBS $ show chatId), ("text", justBS repeatNumberErrorMessage)]
        $ parseRequestThrow_ $
          concat
            [telegramUrl, token, "/sendMessage"]

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
                concat [telegramUrl, token, "/sendMessage"]
          )
  where
    body Environment {..} =
      RequestBodyBS $
        LB.toStrict $
          encode $
            KeyBoard
              chatId
              repeatMessage
              (ReplyMarkup [Text "1", Text "2", Text "3", Text "4", Text "5"])

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
                concat [telegramUrl, token, "/sendMessage"]
          )
  where
    body Environment {..} =
      RequestBodyBS $
        LB.toStrict $
          encode $
            KeyBoard chatId (concat [repeatAcceptMessage, msg, " times"]) RemoveKeyboard

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
        result <- fmap snd <$> runMaybeT (messagesHandle handle isAskedForRepeat repNumber userMsg)
        case result of
          Just HelpMessage -> do
            printLog Release $
              concat
                ["[User]: ", str, "\n [Bot]: ", helpMessage]
            sendHelpMsg chatId
            pure (SendMsgsResult updateId chatIdsForRepeatAcc repeatNumbersAcc)
          Just RepeatMessage -> do
            printLog Release $
              concat
                ["[User]: ", str, "\n Bot]: ", repeatMessage]
            sendRepeatMsg chatId
            pure (SendMsgsResult updateId (chatId : chatIdsForRepeatAcc) repeatNumbersAcc)
          Just (EchoMessage echoRepNumber) -> do
            sendMsg userMsg echoRepNumber
            pure (SendMsgsResult updateId chatIdsForRepeatAcc repeatNumbersAcc)
          Just (RepeatNumberSuccess newRepNumber) -> do
            printLog Release $
              concat
                ["[User]: ", str, "\n Bot]: ", repeatAcceptMessage, show newRepNumber, " times"]
            sendRepeatAcceptMsg chatId str
            pure
              ( SendMsgsResult
                  updateId
                  (filter (/= chatId) chatIdsForRepeatAcc)
                  (Map.insert chatId newRepNumber $ Map.filterWithKey (\k _ -> k /= chatId) repeatNumbersAcc)
              )
          Nothing -> do
            printLog Release $
              concat
                ["[User]: ", str, "\n Bot]: ", repeatNumberErrorMessage]
            sendRepeatNumberErrorMsg chatId
            pure (SendMsgsResult updateId chatIdsForRepeatAcc repeatNumbersAcc)
    )
    (SendMsgsResult (UpdateId 0) chatIdsForRepeat repeatNumbersList)
    userMessages
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
