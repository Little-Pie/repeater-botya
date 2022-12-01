{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Functor.Identity (Identity (runIdentity))
import Environment (Environment (..), LoggingLevel (..))
import Handle (Handle (..), Result (..), messagesHandle)
import qualified System.IO as H (Handle, IOMode (..), hClose, openFile)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

main :: IO ()
main = do
  logHandle <- H.openFile "logFile.txt" H.AppendMode
  hspec $ messagesHandleTest logHandle
  H.hClose logHandle

handle :: Handle Identity String
handle =
  Handle
    { getString = Just,
      sendMessage = \_ _ -> pure (),
      sendText = \_ -> pure (),
      sendRepeat = \_ -> pure (),
      sendRepeatAccept = \_ _ -> pure ()
    }

env :: H.Handle -> Environment
env = Environment "" 30 "" "" 1 "" "" Debug

messagesHandleTest :: H.Handle -> SpecWith ()
messagesHandleTest logHandle =
  describe "Handling messages test" $ do
    it "Should reply helpMessage to \"/help\"" $ do
      let res = runIdentity $ runReaderT (runMaybeT $ messagesHandle handle False 1 "/help") (env logHandle)
      takeFirstInTuple <$> res `shouldBe` pure HelpMessage
    it "Should reply repeatMessage to \"/repeat\"" $ do
      let res = runIdentity $ runReaderT (runMaybeT $ messagesHandle handle False 1 "/repeat") (env logHandle)
      takeFirstInTuple <$> res `shouldBe` pure RepeatMessage
    it "Should text back a message of a user specified number of times" $ do
      let res = runIdentity $ runReaderT (runMaybeT $ messagesHandle handle False 1 "Hello") (env logHandle)
      takeFirstInTuple <$> res `shouldBe` pure EchoMessage
    it "Should reply repeatAcceptMessage if user asked \"/repeat\" in previous message and entered corrected number of repetitions" $ do
      let res = runIdentity $ runReaderT (runMaybeT $ messagesHandle handle True 1 "2") (env logHandle)
      takeFirstInTuple <$> res `shouldBe` pure RepeatNumberSuccess
    it "Should reply repeatNumberErrorMessage if user asked \"/repeat\" in previous message and entered not corrected number of repetitions" $ do
      let res = runIdentity $ runReaderT (runMaybeT $ messagesHandle handle True 1 "7") (env logHandle)
      takeFirstInTuple <$> res `shouldBe` Nothing
    it "Should reply repeatNumberErrorMessage if user asked \"/repeat\" in previous message and sent not text message" $ do
      let res = runIdentity $ runReaderT (runMaybeT $ messagesHandle handle {getString = const Nothing} True 1 "*sticker*") (env logHandle)
      takeFirstInTuple <$> res `shouldBe` Nothing

takeFirstInTuple :: (a, b, c) -> a
takeFirstInTuple (a, _, _) = a
