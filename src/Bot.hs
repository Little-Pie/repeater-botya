{-# LANGUAGE OverloadedStrings #-}

module Bot where

import Parsing
import qualified Data.ByteString as B (ByteString, readFile)
import Data.Aeson
import Network.HTTP.Simple
import Network.HTTP.Client.Internal (ResponseTimeout (ResponseTimeoutMicro))
import Control.Monad (mzero)

data Config = Config {token :: String
                     ,timeout :: Int
                     ,helpMessage :: String
                     ,repeatMessage :: String
                     ,repeatNumber :: Int}

instance FromJSON Config where
  parseJSON (Object config) = Config <$>
                                config .: "token" <*>
                                config .: "timeout" <*>
                                config .: "helpMessage" <*>
                                config .: "repeatMessage" <*>
                                config .: "repeatNumber"
  parseJSON _               = mzero

botLoop :: Int -> Config -> IO ()
botLoop offset config = do
  telegramResponse <- getUpdates offset config
  let mbUpdates = parseUpdates telegramResponse
  case mbUpdates of
    Nothing -> putStrLn "Couldn't parse telegramResponse"
    Just updates -> do
      newOffset <- sendMsgs updates config
      botLoop newOffset config

getUpdates :: Int -> Config -> IO B.ByteString
getUpdates offset config = do
    response <- httpBS $ setRequestResponseTimeout (ResponseTimeoutMicro $ (timeout config + 1)*1000000) $ parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token config ++ "/getUpdates?offset=" ++ show offset ++ "&timeout=" ++ show (timeout config)
    return (getResponseBody response)

parseUpdates :: B.ByteString -> Maybe TelegramUpdates
parseUpdates telegramResponse = decodeStrict telegramResponse

sendMsgs :: TelegramUpdates -> Config -> IO Int
sendMsgs (TelegramUpdates userMessages) config = do
  case userMessages of
    [] -> return 0
    [x] -> case x of
      TextMessage updateId chatId msg -> case msg of
        "/help" -> do
          sendHelpMsg chatId config
          return  (updateId + 1) 
        _       -> do
          sendTextMsg chatId msg config
          return  (updateId + 1)
      StickerMessage updateId chatId fileId -> do
        sendStickerMsg chatId fileId config
        return  (updateId + 1)
    (x:xs) -> case x of
      TextMessage updateId chatId msg -> do
        sendTextMsg chatId msg config
        sendMsgs (TelegramUpdates xs) config
      StickerMessage updateId chatId fileId -> do
        sendTextMsg chatId fileId config
        sendMsgs (TelegramUpdates xs) config

sendTextMsg :: Int -> String -> Config -> IO ()
sendTextMsg chatid msg config = do
    mapM httpNoBody $ replicate (repeatNumber config) $ (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token config ++ "/sendMessage?chat_id=" ++ show chatid ++ "&text=" ++ msg)
    return ()

sendStickerMsg :: Int -> String -> Config -> IO ()
sendStickerMsg chatid stickerID config = do
    mapM httpNoBody $ replicate (repeatNumber config) $ (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token config ++ "/sendSticker?chat_id=" ++ show chatid ++ "&sticker=" ++ stickerID)
    return ()

sendHelpMsg :: Int -> Config -> IO ()
sendHelpMsg chatid config = do
    httpNoBody (parseRequestThrow_ $ "https://api.telegram.org/bot" ++ token config ++ "/sendMessage?chat_id=" ++ show chatid ++ "&text=" ++ helpMessage config)
    return ()

getConfig :: IO (Maybe Config)
getConfig = do
  rawJSON <- B.readFile "config.json"
  let result = decodeStrict rawJSON :: Maybe Config
  case result of
    Nothing -> return Nothing
    Just conf -> return $ Just conf