{-# LANGUAGE TemplateHaskell #-}
module UnitTest.CallbackParse.PostbackCallback where


import Data.Aeson (Value)
import Data.Yaml.TH (decodeFile)

import Test.Tasty as Tasty
import Test.Tasty.HUnit as Tasty
import Web.Facebook.Messenger

import UnitTest.Internal

--------------
-- POSTBACK --
--------------

postbackCallbackVal :: Value
postbackCallbackVal = $$(decodeFile "test/json/callback/postback_callback.json")

postbackReferralVal :: Value
postbackReferralVal = $$(decodeFile "test/json/callback/postback_callback_referral.json")

postbackSecondaryVal :: Value
postbackSecondaryVal = $$(decodeFile "test/json/callback/postback_callback_secondary.json")

postbackTests :: TestTree
postbackTests = Tasty.testGroup "Postback Callbacks"
    [ postbackCallback
    , postbackCallbackReferral
    , postbackSecondary
    ]

postbackCallback :: TestTree
postbackCallback = testCase "Regular postback" $
    eParse postbackCallbackVal @?= Right expected
  where expected = msg $ CMPostback $ PBRegular
                    $ RegularPostback (Just "Aan de slag") "getstarted" Nothing

postbackCallbackReferral :: TestTree
postbackCallbackReferral = testCase "Regular postback w/ referral" $
    eParse postbackReferralVal @?= Right expected
  where expected = msg $ CMPostback $ PBRegular
                    $ RegularPostback (Just "Aan de slag") "getstarted" $ Just
                        $ ReferralLink $ RefShortLink "some_cta_or_another"

postbackSecondary :: TestTree
postbackSecondary = testCase "Secondary postback" $
    eParse postbackSecondaryVal @?= Right expected
  where expected = msg $ CMPostback $ PBSecondary
                    $ SecondaryPostback Nothing $ Just
                        $ ReferralChat $ RefChatPlugin (Just "<REF_DATA_PASSED_IN_CODE>")
                                                       "https://some.website.com/chat/page.html"


msg :: CallbackContent -> CallbackMessaging
msg contnt = standardMessaging (Just 1520151355121)
                               Nothing
                               contnt