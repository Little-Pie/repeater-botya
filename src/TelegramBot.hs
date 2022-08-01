{-# LANGUAGE OverloadedStrings #-}

module TelegramBot where

import Parsing
import Data.Aeson
import qualified Data.ByteString as B (ByteString, readFile)
import Network.HTTP.Simple
import Network.HTTP.Client.Internal (ResponseTimeout (ResponseTimeoutMicro), RequestBody (RequestBodyBS))
import qualified Data.ByteString.Char8 as BC
import Text.Read (readMaybe)

type RepeatNumbers = [(Int,Int)]

botLoop :: Int -> Config -> [Int] -> RepeatNumbers -> IO ()
botLoop offset config chatIdsForRepeat repeatNumbers = do
  telegramResponse <- getUpdates offset config
  let mbUpdates = parseUpdates telegramResponse
  case mbUpdates of
    Nothing -> putStrLn "Couldn't parse telegramResponse"
    Just updates -> do
      (newOffset, newChatIdsForRepeat, newRepeatNumbers) <- sendMsgs updates config chatIdsForRepeat repeatNumbers
      botLoop newOffset config newChatIdsForRepeat newRepeatNumbers

getUpdates :: Int -> Config -> IO B.ByteString
getUpdates offset config = do
    response <- httpBS $ setRequestResponseTimeout (ResponseTimeoutMicro $ (timeout config + 1)*1000000) $ parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token config ++ "/getUpdates?offset=" ++ show offset ++ "&timeout=" ++ show (timeout config)
    return (getResponseBody response)

parseUpdates :: B.ByteString -> Maybe TelegramUpdates
parseUpdates telegramResponse = decodeStrict telegramResponse

sendMsgs :: TelegramUpdates -> Config -> [Int] -> RepeatNumbers -> IO (Int,[Int],RepeatNumbers)
sendMsgs (TelegramUpdates userMessages) config chatIdsForRepeat repeatNumbers = do
  case userMessages of
    [] -> return (0,chatIdsForRepeat,repeatNumbers)
    [x] -> case x of
      TextMessage updateId chatId msg -> case msg of
        "/help" -> do
          sendHelpMsg chatId config
          return  (updateId + 1, chatIdsForRepeat, repeatNumbers)
        "/repeat" -> do
          sendRepeatMsg chatId config
          return  (updateId + 1, chatId:chatIdsForRepeat, repeatNumbers)
        _       -> if chatId `notElem` chatIdsForRepeat then do
          sendTextMsg chatId msg config chatIdsForRepeat repeatNumbers
          return  (updateId + 1, chatIdsForRepeat, repeatNumbers)
          else do
            case readMaybe msg :: Maybe Int of
              Nothing -> do
                sendErrorMsg chatId config
                sendRepeatMsg chatId config
                return (updateId + 1, chatIdsForRepeat, repeatNumbers)
              Just numb -> do
                if numb > 0 && numb < 6 then do
                  sendRepeatAcceptMsg chatId msg config
                  return (updateId + 1, (filter (/= chatId) chatIdsForRepeat), (chatId,read msg :: Int):repeatNumbers)
                else do
                  sendErrorMsg chatId config
                  sendRepeatMsg chatId config
                  return (updateId + 1, chatIdsForRepeat, repeatNumbers)
      StickerMessage updateId chatId fileId -> do
        sendStickerMsg chatId fileId config repeatNumbers
        return  (updateId + 1, chatIdsForRepeat, repeatNumbers)
      NothingMessage updateId -> return (updateId + 1, chatIdsForRepeat, repeatNumbers)
    (x:xs) -> case x of
      TextMessage updateId chatId msg -> case msg of
        "/help" -> do
          sendHelpMsg chatId config
          sendMsgs (TelegramUpdates xs) config chatIdsForRepeat repeatNumbers
        "/repeat" -> do
          sendRepeatMsg chatId config
          sendMsgs (TelegramUpdates xs) config (chatId:chatIdsForRepeat) repeatNumbers
        _       -> if chatId `notElem` chatIdsForRepeat then do
          sendTextMsg chatId msg config chatIdsForRepeat repeatNumbers
          sendMsgs (TelegramUpdates xs) config chatIdsForRepeat repeatNumbers
          else do
            sendRepeatAcceptMsg chatId msg config
            sendMsgs (TelegramUpdates xs) config (filter (/= chatId) chatIdsForRepeat) ((chatId,read msg :: Int):repeatNumbers)
      StickerMessage updateId chatId fileId -> do
        sendStickerMsg chatId fileId config repeatNumbers
        sendMsgs (TelegramUpdates xs) config chatIdsForRepeat repeatNumbers
      NothingMessage updateId -> sendMsgs (TelegramUpdates xs) config chatIdsForRepeat repeatNumbers

sendTextMsg :: Int -> String -> Config -> [Int] -> RepeatNumbers -> IO ()
sendTextMsg chatid msg config chatIdsForRepeat repeatNumbers = do
    mapM httpNoBody $ replicate (case lookup chatid repeatNumbers of
                                   Nothing -> repeatNumber config
                                   Just repeatNumb -> repeatNumb) $ (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token config ++ "/sendMessage?chat_id=" ++ show chatid ++ "&text=" ++ msg)
    return ()

sendStickerMsg :: Int -> String -> Config -> RepeatNumbers -> IO ()
sendStickerMsg chatid stickerID config repeatNumbers = do
    mapM httpNoBody $ replicate (case lookup chatid repeatNumbers of
                                   Nothing -> repeatNumber config
                                   Just repeatNumb -> repeatNumb) $ (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token config ++ "/sendSticker?chat_id=" ++ show chatid ++ "&sticker=" ++ stickerID)
    return ()

sendHelpMsg :: Int -> Config -> IO ()
sendHelpMsg chatid config = do
    httpNoBody (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token config ++ "/sendMessage?chat_id=" ++ show chatid ++ "&text=" ++ helpMessage config)
    return ()

sendErrorMsg :: Int -> Config -> IO ()
sendErrorMsg chatid config = do
    httpNoBody (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token config ++ "/sendMessage?chat_id=" ++ show chatid ++ "&text=" ++ errorMessage config)
    return ()

sendRepeatMsg :: Int -> Config -> IO ()
sendRepeatMsg chatid config = do
    resp <- httpBS $
      addRequestHeader "Content-Type" "application/json" $
      setRequestBody body
        (setRequestMethod "POST" $
          parseRequestThrow_ $
          "https://api.telegram.org/bot" ++ token config ++ "/sendMessage")
    return ()
    where body = RequestBodyBS $ BC.pack $ "{\"chat_id\":\"" ++ show chatid ++ "\",\"text\":\"" ++ repeatMessage config ++ "\",\"reply_markup\":{\"keyboard\": [[{\"text\": \"1\"},{\"text\": \"2\"},{\"text\": \"3\"},{\"text\": \"4\"},{\"text\": \"5\"}]]},{\"one_time_keyboard\":\"true\"}}"    

sendRepeatAcceptMsg :: Int -> String -> Config -> IO ()
sendRepeatAcceptMsg chatid msg config = do
    resp <- httpBS $
      addRequestHeader "Content-Type" "application/json" $
      setRequestBody body
        (setRequestMethod "POST" $
          parseRequestThrow_ $
          "https://api.telegram.org/bot" ++ token config ++ "/sendMessage")
    return ()
    where body = RequestBodyBS $ BC.pack $ "{\"chat_id\":\"" ++ show chatid ++ "\",\"text\":\"" ++ repeatAcceptMessage config ++ msg ++ " times\",\"reply_markup\":{\"remove_keyboard\":true}"

getConfig :: IO (Maybe Config)
getConfig = do
  rawJSON <- B.readFile "config.json"
  let result = decodeStrict rawJSON :: Maybe Config
  case result of
    Nothing -> return Nothing
    Just conf -> return $ Just conf