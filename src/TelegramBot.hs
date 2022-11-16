{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TelegramBot where

import Control.Monad (replicateM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (decodeStrict, encode)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BSC (pack)
import qualified Data.ByteString.Lazy as LB (toStrict)
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
import Types.FromJSON (TelegramUpdates (..), UserMessage (..))
import Types.ToJSON (KeyBoard (..), Keys (..), ReplyMarkup (..))

type RepeatNumbers = [(Int, Int)]

justBS :: String -> Maybe BS.ByteString
justBS = Just . BSC.pack

botTokenCheck :: App ()
botTokenCheck = do
  Environment {..} <- ask
  void $ liftIO $ httpBS $ parseRequestThrow_ $ concat ["https://api.telegram.org/bot", token, "/getMe"]

runTelegramBot :: App ()
runTelegramBot = do
  botTokenCheck
  telegramBotLoop 0 [] []

telegramBotLoop :: Int -> [Int] -> RepeatNumbers -> App ()
telegramBotLoop offset chatIdsForRepeat repeatNumbers = do
  telegramResponse <- getUpdates offset
  let mbUpdates = decodeStrict telegramResponse :: Maybe TelegramUpdates
  case mbUpdates of
    Nothing -> do
      printLog Error "Couldn't parse telegramResponse"
      liftIO $ putStrLn "Couldn't parse telegramResponse"
    Just updates -> do
      (newOffset, newChatIdsForRepeat, newRepeatNumbers) <- sendMsgs updates chatIdsForRepeat repeatNumbers
      telegramBotLoop newOffset newChatIdsForRepeat newRepeatNumbers

getUpdates :: Int -> App BS.ByteString
getUpdates offset = do
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
                ["https://api.telegram.org/bot", token, "/getUpdates"]
  pure (getResponseBody response)

sendMsg :: UserMessage -> Int -> App ()
sendMsg userMsg repNumber = do
  Environment {..} <- ask
  case userMsg of
    TextMessage _ chatId msg -> do
      printLog Release $ concat ["[User]: ", msg]
      replicateM_ repNumber $ do
        printLog Release $ concat ["[Bot]: ", msg]
        (liftIO . httpNoBody) $
          setRequestQueryString
            [("chat_id", justBS $ show chatId), ("text", justBS msg)]
            $ parseRequestThrow_ $
              concat
                ["https://api.telegram.org/bot", token, "/sendMessage"]
    StickerMessage _ chatId stickerId -> do
      printLog Release $ concat ["[User]: *some sticker with id ", stickerId, "*"]
      replicateM_ repNumber $ do
        printLog Release $ concat ["[Bot]: *some sticker with id ", stickerId, "*"]
        (liftIO . httpNoBody) $
          setRequestQueryString
            [("chat_id", justBS $ show chatId), ("sticker", justBS stickerId)]
            $ parseRequestThrow_ $
              concat
                ["https://api.telegram.org/bot", token, "/sendSticker"]
    NothingMessage _ _ -> do
      printLog Warning "Warning: User sent unknown type of message"

sendHelpMsg :: Int -> App ()
sendHelpMsg chatId = do
  Environment {..} <- ask
  void . liftIO $
    httpNoBody $
      setRequestQueryString
        [("chat_id", justBS $ show chatId), ("text", justBS helpMessage)]
        $ parseRequestThrow_ $
          concat
            ["https://api.telegram.org/bot", token, "/sendMessage"]

sendRepeatNumberErrorMsg :: Int -> App ()
sendRepeatNumberErrorMsg chatId = do
  Environment {..} <- ask
  void . liftIO $
    httpNoBody $
      setRequestQueryString
        [("chat_id", justBS $ show chatId), ("text", justBS repeatNumberErrorMessage)]
        $ parseRequestThrow_ $
          concat
            ["https://api.telegram.org/bot", token, "/sendMessage"]

sendRepeatMsg :: Int -> App ()
sendRepeatMsg chatId = do
  env@Environment {..} <- ask
  void . liftIO $
    httpBS $
      addRequestHeader "Content-Type" "application/json" $
        setRequestBody
          (body env)
          ( setRequestMethod "POST" $
              parseRequestThrow_ $
                concat ["https://api.telegram.org/bot", token, "/sendMessage"]
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

sendRepeatAcceptMsg :: Int -> String -> App ()
sendRepeatAcceptMsg chatId msg = do
  env@Environment {..} <- ask
  void . liftIO $
    httpBS $
      addRequestHeader "Content-Type" "application/json" $
        setRequestBody
          (body env)
          ( setRequestMethod "POST" $
              parseRequestThrow_ $
                concat ["https://api.telegram.org/bot", token, "/sendMessage"]
          )
  where
    body Environment {..} =
      RequestBodyBS $
        LB.toStrict $
          encode $
            KeyBoard chatId (concat [repeatAcceptMessage, msg, " times"]) RemoveKeyboard

sendMsgs :: TelegramUpdates -> [Int] -> RepeatNumbers -> App (Int, [Int], RepeatNumbers)
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
          printLog Release $
            concat
              ["[User]: ", str, "\n [Bot]: ", helpMessage]
          sendHelpMsg chatId
          pure (updateId + 1, chatIdsForRepeat, repeatNumbers)
        RepeatMessage -> do
          printLog Release $
            concat
              ["[User]: ", str, "\n Bot]: ", repeatMessage]
          sendRepeatMsg chatId
          pure (updateId + 1, chatId : chatIdsForRepeat, repeatNumbers)
        EchoMessage echoRepNumber -> do
          sendMsg userMsg echoRepNumber
          pure (updateId + 1, chatIdsForRepeat, repeatNumbers)
        RepeatNumberSuccess newRepNumber -> do
          printLog Release $
            concat
              ["[User]: ", str, "\n Bot]: ", repeatAcceptMessage, show newRepNumber, " times"]
          sendRepeatAcceptMsg chatId str
          pure
            ( updateId + 1,
              filter (/= chatId) chatIdsForRepeat,
              (chatId, read str :: Int) : filter (\a -> fst a /= chatId) repeatNumbers
            )
        WrongRepeatNumber -> do
          printLog Release $
            concat
              ["[User]: ", str, "\n Bot]: ", repeatNumberErrorMessage]
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
          printLog Release $
            concat
              ["[User]: ", str, "\n [Bot]: ", helpMessage]
          sendHelpMsg chatId
          sendMsgs (TelegramUpdates userMsgs) chatIdsForRepeat repeatNumbers
        RepeatMessage -> do
          printLog Release $
            concat
              ["[User]: ", str, "\n Bot]: ", repeatMessage]
          sendRepeatMsg chatId
          sendMsgs (TelegramUpdates userMsgs) (chatId : chatIdsForRepeat) repeatNumbers
        EchoMessage echoRepNumber -> do
          printLog Release $
            concat
              ["[User]: ", str]
          replicateM_ echoRepNumber $ printLog Release $ concat ["[Bot]: ", str]
          sendMsg userMsg echoRepNumber
          sendMsgs (TelegramUpdates userMsgs) chatIdsForRepeat repeatNumbers
        RepeatNumberSuccess newRepNumber -> do
          printLog Release $
            concat
              ["[User]: ", str, "\n Bot]: ", repeatAcceptMessage, show newRepNumber, " times"]
          sendRepeatAcceptMsg chatId str
          sendMsgs
            (TelegramUpdates userMsgs)
            (filter (/= chatId) chatIdsForRepeat)
            ((chatId, read str :: Int) : filter (\a -> fst a /= chatId) repeatNumbers)
        WrongRepeatNumber -> do
          printLog Release $ concat ["[User]: ", str, "\n Bot]: ", repeatNumberErrorMessage]
          sendRepeatNumberErrorMsg chatId
          sendMsgs (TelegramUpdates userMsgs) chatIdsForRepeat repeatNumbers
  where
    handle =
      Handle
        { getString = getMessage
        }

getChatId :: UserMessage -> Int
getChatId (TextMessage _ chatId _) = chatId
getChatId (StickerMessage _ chatId _) = chatId
getChatId (NothingMessage _ chatId) = chatId

getUpdateId :: UserMessage -> Int
getUpdateId (TextMessage updateId _ _) = updateId
getUpdateId (StickerMessage updateId _ _) = updateId
getUpdateId (NothingMessage updateId _) = updateId

getMessage :: UserMessage -> Maybe String
getMessage (TextMessage _ _ str) = Just str
getMessage (StickerMessage _ _ str) = Just str
getMessage (NothingMessage _ _) = Nothing
