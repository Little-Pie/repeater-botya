module Main where

import Bot

main :: IO ()
main = do
  mbConfig <- getConfig
  case mbConfig of
    Nothing -> putStrLn "Couldn't parse config"
    Just fromConfig  -> do
      botLoop 0 fromConfig [] []
