{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TelegramBot where

import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (decodeStrict, encode)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB (toStrict)
import Data.Maybe (fromMaybe)
import Environment (App, Environment (..))
import Handle (Handle (..), Result (..), messagesHandle)
import Logging (printDebug, printError, printRelease, printWarning)
import Network.HTTP.Client.Internal (RequestBody (RequestBodyBS), ResponseTimeout (ResponseTimeoutMicro))
import Network.HTTP.Simple (addRequestHeader, getResponseBody, httpBS, httpNoBody, parseRequestThrow_, setRequestBody, setRequestMethod, setRequestResponseTimeout)
import Text.Read (readMaybe)
import Types.FromJSON (TelegramUpdates (..), UserMessage (..))
import Types.ToJSON (KeyBoard (..), Keys (..), ReplyMarkup (..))

type RepeatNumbers = [(Int, Int)]

telegramBotLoop :: Int -> [Int] -> RepeatNumbers -> App ()
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

getUpdates :: Int -> App BS.ByteString
getUpdates offset = do
  Environment {..} <- ask
  response <- liftIO $ httpBS $ setRequestResponseTimeout (ResponseTimeoutMicro $ (timeout + 1) * 1000000) $ parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/getUpdates?offset=" ++ show offset ++ "&timeout=" ++ show timeout
  return (getResponseBody response)

sendMsg :: UserMessage -> Int -> App ()
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

sendHelpMsg :: Int -> App ()
sendHelpMsg chatId = do
  Environment {..} <- ask
  liftIO $ httpNoBody (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/sendMessage?chat_id=" ++ show chatId ++ "&text=" ++ helpMessage)
  pure ()

sendRepeatNumberErrorMsg :: Int -> App ()
sendRepeatNumberErrorMsg chatId = do
  Environment {..} <- ask
  liftIO $ httpNoBody (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token ++ "/sendMessage?chat_id=" ++ show chatId ++ "&text=" ++ repeatNumberErrorMessage)
  pure ()

sendRepeatMsg :: Int -> App ()
sendRepeatMsg chatId = do
  env@Environment {..} <- ask
  resp <-
    liftIO $
      httpBS $
        addRequestHeader "Content-Type" "application/json" $
          setRequestBody
            (body env)
            ( setRequestMethod "POST" $
                parseRequestThrow_ $
                  "https://api.telegram.org/bot" ++ token ++ "/sendMessage"
            )
  pure ()
  where
    body Environment {..} = RequestBodyBS $ LB.toStrict $ encode $ KeyBoard chatId repeatMessage (ReplyMarkup [Text "1", Text "2", Text "3", Text "4", Text "5"])

sendRepeatAcceptMsg :: Int -> String -> App ()
sendRepeatAcceptMsg chatId msg = do
  env@Environment {..} <- ask
  resp <-
    liftIO $
      httpBS $
        addRequestHeader "Content-Type" "application/json" $
          setRequestBody
            (body env)
            ( setRequestMethod "POST" $
                parseRequestThrow_ $
                  "https://api.telegram.org/bot" ++ token ++ "/sendMessage"
            )
  pure ()
  where
    body Environment {..} = RequestBodyBS $ LB.toStrict $ encode $ KeyBoard chatId (repeatAcceptMessage ++ msg ++ " times") RemoveKeyboard

sendMsgs :: TelegramUpdates -> [Int] -> RepeatNumbers -> App (Int, [Int], RepeatNumbers)
sendMsgs (TelegramUpdates userMessages) chatIdsForRepeat repeatNumbers = do
  Environment {..} <- ask
  case userMessages of
    [] -> return (0, chatIdsForRepeat, repeatNumbers)
    [userMsg] -> do
      let chatId = getChatId userMsg
      let updateId = getUpdateId userMsg
      let isAskedForRepeat = getChatId userMsg `elem` chatIdsForRepeat
      let repNumber = fromMaybe repeatNumber (lookup chatId repeatNumbers)
      let str = fromMaybe "" (getMessage userMsg)
      (newIsAskedForRepeat, res) <- messagesHandle handle isAskedForRepeat repNumber userMsg
      case res of
        HelpMessage -> do
          printRelease $ "[User]: " ++ str ++ "\n [Bot]: " ++ helpMessage
          sendHelpMsg chatId
          return (updateId + 1, chatIdsForRepeat, repeatNumbers)
        RepeatMessage -> do
          printRelease $ "[User]: " ++ str ++ "\n Bot]: " ++ repeatMessage
          sendRepeatMsg chatId
          return (updateId + 1, chatId : chatIdsForRepeat, repeatNumbers)
        EchoMessage echoRepNumber -> do
          sendMsg userMsg echoRepNumber
          return (updateId + 1, chatIdsForRepeat, repeatNumbers)
        RepeatNumberSuccess newRepNumber -> do
          printRelease $ "[User]: " ++ str ++ "\n Bot]: " ++ repeatAcceptMessage ++ show newRepNumber ++ " times"
          sendRepeatAcceptMsg chatId str
          return (updateId + 1, filter (/= chatId) chatIdsForRepeat, (chatId, read str :: Int) : filter (\a -> fst a /= chatId) repeatNumbers)
        WrongRepeatNumber -> do
          printRelease $ "[User]: " ++ str ++ "\n Bot]: " ++ repeatNumberErrorMessage
          sendRepeatNumberErrorMsg chatId
          return (updateId + 1, chatIdsForRepeat, repeatNumbers)
    (userMsg : userMsgs) -> do
      let chatId = getChatId userMsg
      let isAskedForRepeat = getChatId userMsg `elem` chatIdsForRepeat
      let repNumber = fromMaybe repeatNumber (lookup chatId repeatNumbers)
      let str = fromMaybe "" (getMessage userMsg)
      (newIsAskedForRepeat, res) <- messagesHandle handle isAskedForRepeat repNumber userMsg
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
