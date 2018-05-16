module UnitTest.Internal where


import Data.Aeson (FromJSON(..), Value)
import Data.Aeson.Types (parseEither)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit ((@?=), testCase)

import Web.Facebook.Messenger


pageID :: PageID
pageID = PageID "234554838031189"

standardRecipient :: CallbackRecipient
standardRecipient = CallbackRecipient pageID

psid :: PSID
psid = PSID "1739141089378194"

standardSender :: Maybe CallbackSender
standardSender = Just $ CallbackSender psid

standardMessaging :: Maybe Integer -> Maybe PriorMessage -> CallbackContent -> CallbackMessaging
standardMessaging = CallbackMessaging standardSender standardRecipient

eParse :: FromJSON a => Value -> Either String a
eParse = parseEither parseJSON

parseTest :: (FromJSON a, Eq a, Show a) => TestName -> Value -> a -> TestTree
parseTest testName val = testCase testName . (eParse val @?=) . Right
