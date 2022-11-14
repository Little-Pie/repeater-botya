{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Functor.Identity (Identity)
import Handle (Handle (..), Result (..), messagesHandle)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  messagesHandleTest

handle :: Handle String
handle =
  Handle
    { getString = Just
    }

messagesHandleTest :: SpecWith ()
messagesHandleTest =
  describe "Handling messages test" $ do
    it "Should reply helpMessage to \"/help\"" $ do
      res <- runMaybeT $ messagesHandle handle False 1 "/help"
      snd <$> res `shouldBe` pure HelpMessage
    it "Should reply repeatMessage to \"/repeat\"" $ do
      res <- runMaybeT $ messagesHandle handle False 1 "/repeat"
      snd <$> res `shouldBe` pure RepeatMessage
    it "Should text back a message of a user specified number of times" $ do
      res <- runMaybeT $ messagesHandle handle False 1 "Hello"
      snd <$> res `shouldBe` pure (EchoMessage 1)
    it "Should reply repeatAcceptMessage if user asked \"/repeat\" in previous message and entered corrected number of repetitions" $ do
      res <- runMaybeT $ messagesHandle handle True 1 "2"
      snd <$> res `shouldBe` pure (RepeatNumberSuccess 2)
    it "Should reply repeatNumberErrorMessage if user asked \"/repeat\" in previous message and entered not corrected number of repetitions" $ do
      res <- runMaybeT $ messagesHandle handle True 1 "7"
      snd <$> res `shouldBe` Nothing
    it "Should reply repeatNumberErrorMessage if user asked \"/repeat\" in previous message and sent not text message" $ do
      res <- runMaybeT $ messagesHandle handle {getString = const Nothing} True 1 "*sticker*"
      snd <$> res `shouldBe` Nothing
