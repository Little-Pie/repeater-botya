{-# LANGUAGE OverloadedStrings #-}

module Lib where

-- import           Parsing
-- import           Network.HTTP.Simple                  ( setRequestResponseTimeout, parseRequestThrow_, Request, httpNoBody, httpBS, getResponseBody )
-- import           Network.HTTP.Client.Internal         ( ResponseTimeout (ResponseTimeoutMicro) )
-- import qualified Data.ByteString               as B
-- import           Data.Aeson
-- import           Data.Aeson.Types
-- import           Data.Aeson.Key
-- import           Text.Read
-- import           System.Timeout
-- import           Data.List

-- data Times = Times {times :: Int} deriving (Show)

-- helper :: Times -> Int -> [Int] -> String -> IO Times
-- helper ti offset chatid "/help" = sendMsg "This bot texts your messages back" chatid >> pure ti
-- helper ti offset chatid "/repeat" = ifBadRep ti offset chatid
-- helper ti offset chatid str = repStr (times ti) str
--   where repStr :: Int -> String -> IO Times
--         repStr 0 _ = pure ti
--         repStr n str = do
--           sendMsg str chatid
--           repStr (n - 1) str
--           pure ti

-- ifBadRep ti offset chatid = do
--   sendMsg "Enter a number of repetition:" chatid
--   nStr <- getUpdateMsg offset chatid
--   let mbN = readMaybe nStr :: Maybe Int
--   case mbN of
--     Just n -> if n>0 && n<6 then pure (ti {times = n}) else sendMsg warn chatid >> pure ti >> ifBadRep ti offset chatid
--     Nothing -> sendMsg warn chatid >> pure ti >> ifBadRep ti offset chatid
--   where warn = "You gave us not very good number. It should be from 1 to 5. Please, try again"

-- sendMsg msg chatid = do
--   a <- httpNoBody (parseRequestThrow_ $ "https://api.telegram.org/bot5474987503:AAEHAnVep_qEDtS_qqqVZrTzdoWuQw_GPoc/sendMessage?chat_id=" ++ show chatid ++ "&text=" ++ msg)
--   return ()

-- getUpdateMsg :: Int -> [Int] -> IO [[String]]
-- getUpdateMsg offset chatid = do
--   resp <- httpBS (setRequestResponseTimeout (ResponseTimeoutMicro 31000000) $ parseRequestThrow_ $ "https://api.telegram.org/bot5474987503:AAEHAnVep_qEDtS_qqqVZrTzdoWuQw_GPoc/getUpdates?offset=" ++ show offset ++ "&timeout=30")
--   let result = decodeStrict $ getResponseBody resp :: Maybe TelegramUpdates
--   return $ case result of
--     Nothing   -> [[""]]
--     Just TextMessage -> case updates TextMessage of
--       [] -> [[""]]
--       xs -> map (\x -> map message x) $ map (\x -> chatIDfilter x $ updates TextMessage) chatid

-- getUpdateID :: IO Int
-- getUpdateID = do
--   resp <- httpBS "https://api.telegram.org/bot5474987503:AAEHAnVep_qEDtS_qqqVZrTzdoWuQw_GPoc/getUpdates"
--   let result = decodeStrict $ getResponseBody resp :: Maybe TelegramUpdates
--   return $ case result of
--     Nothing   -> 0
--     Just TextMessage -> case updates TextMessage of
--       [] -> 0
--       xs -> maximum $ map updateID $ updates TextMessage

-- chatIDfilter :: Int -> [TextMessage] -> [TextMessage]
-- chatIDfilter _ [] = []
-- chatIDfilter chatid TextMessages = filter (\um -> chatID um == chatid) TextMessages

-- getChatID :: IO [Int]
-- getChatID = do
--   resp <- httpBS "https://api.telegram.org/bot5474987503:AAEHAnVep_qEDtS_qqqVZrTzdoWuQw_GPoc/getUpdates"
--   let result = decodeStrict $ getResponseBody resp :: Maybe TelegramUpdates
--   return $ case result of
--     Nothing   -> [0]
--     Just TextMessage -> case updates TextMessage of
--       [] -> [0]
--       xs -> removeSame $ map chatID $ updates TextMessage

-- removeSame :: (Eq a) => [a] -> [a]
-- removeSame [] = [] 
-- removeSame (x:xs) = if x `notElem` xs then x:removeSame xs else removeSame xs