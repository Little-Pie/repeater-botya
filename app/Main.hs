module Main where

import Bot

main :: IO ()
main = do
  mbConfig <- getConfig
  case mbConfig of
    Nothing -> putStrLn "Couldn't parse config"
    Just fromConfig  -> do
      botLoop 0 fromConfig
  -- let ti = Times 1
  -- saying ti 0
  --   where
  --   saying :: Times -> Int -> IO ()
  --   saying t offset = do
  --       chatid <- getChatID
  --       text <- getUpdateMsg offset chatid
  --       newti <- helper t offset chatid text
  --       saying newti (offset + 1)
  --       pure ()
