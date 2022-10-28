{-# LANGUAGE OverloadedStrings #-}

import Data.Functor.Identity (Identity)
import Handle (Result (..), consolBotHandle)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  consolBotHandleTest

consolBotHandleTest :: SpecWith ()
consolBotHandleTest =
  describe "Handling messages test" $ do
    it "Should reply helpMessage to \"/help\"" $ do
      let res = consolBotHandle False 1 "/help" :: Identity (Bool, Result)
      snd <$> res `shouldBe` pure HelpMessage
    it "Should reply repeatMessage to \"/repeat\"" $ do
      let res = consolBotHandle False 1 "/repeat" :: Identity (Bool, Result)
      snd <$> res `shouldBe` pure RepeatMessage
    it "Should text back a message of a user specified number of times" $ do
      let res = consolBotHandle False 1 "Hello" :: Identity (Bool, Result)
      snd <$> res `shouldBe` pure (EchoMessage 1)
    it "Should reply repeatAcceptMessage if user asked \"/repeat\" in previous message and entered corrected number of repetitions" $ do
      let res = consolBotHandle True 1 "2" :: Identity (Bool, Result)
      snd <$> res `shouldBe` pure (RepeatNumberSuccess 2)
    it "Should reply repeatNumberErrorMessage if user asked \"/repeat\" in previous message and entered not corrected number of repetitions" $ do
      let res = consolBotHandle True 1 "7" :: Identity (Bool, Result)
      snd <$> res `shouldBe` pure WrongRepeatNumber
    it "Should reply repeatNumberErrorMessage if user asked \"/repeat\" in previous message and entered a string instead of number of repetitions" $ do
      let res = consolBotHandle True 1 "Hello" :: Identity (Bool, Result)
      snd <$> res `shouldBe` pure WrongRepeatNumberString
