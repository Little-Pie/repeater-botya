module ConsolBot where

import Text.Read

consolBotLoop :: IO ()
consolBotLoop = do
  saying 1
  where
    saying :: Int -> IO ()
    saying t = do
      text <- getLine
      newt <- helper t text
      saying newt
      pure ()

helper :: Int -> String -> IO Int
helper t "/help" = putStrLn "This bot texts your messages back" >> pure t
helper t "/repeat" = ifBad t
helper t str = mapM putStrLn (replicate t str) >> pure t

ifBad t = do
  putStrLn "Enter a number of repetition:"
  nStr <- getLine
  let mbN = readMaybe nStr :: Maybe Int
  case mbN of
    Just n -> if n > 0 && n < 6 then pure n else putStrLn msg >> pure t >> ifBad t
    Nothing -> putStrLn msg >> pure t >> ifBad t

msg = "Please enter number from 1 to 5"
